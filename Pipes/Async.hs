{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pipes.Async where

import Control.Exception (AsyncException(..))
import Control.Concurrent.Lifted
import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM
import Control.Monad (liftM)
import Control.Monad.Trans.Control
import Pipes
import Pipes.Internal
import Pipes.Safe

-- | A substitute for 'Pipes.>->' that executes both the upstream producer and
-- the downstream consumer in separate threads (see '>&>' for an operator
-- version, with a default queue size of 16 slots). The reason separate
-- threads are used for both sides is so that the current thread (running
-- 'runEffect' or 'toListM', for example) can manage the bidirectional
-- semantics for the resulting Proxy. That is:
--
-- Upstream is executed in task A, downstream in task B, and 'runEffect' in
-- the parent thread. Tasks A and B are connected so that 'b' values produced
-- in A are immediately enqueued and available to B. 'runEffect' does not manage
-- passing 'b' values from A to B, as it normally would; rather they flow
-- directly through a 'TBQueue' side-channel.
--
-- If upstream should attempt to send an @a'@ value further upstream,
-- expecting an 'a' in return, this will block task A as 'runEffect' sends the
-- request further up the chain. Or, should downstream send a 'c' value
-- downstream and expect a @c'@, it will blocks task B as 'runEffect' sends
-- the response further down the chain.
--
-- If upstream exits, its result value is enqueued until downstream sees it,
-- at which point 'runEffect' terminates with this value. However, if
-- downstream should exit first, this result is communicated directly to
-- 'runEffect', which returns it immediately, canceling both threads. Thus,
-- execution lifetime is biased toward the downstream consumer, since it is
-- more likely that downstream will consume elements until there are none
-- left, than that upstream would produce elements while waiting for
-- downstream to terminate.
--
-- If an exception occurs in either upstream or downstream, it is re-thrown in
-- the 'runEffect' thread. Also, no matter what happens, both the upstream and
-- downstream threads are canceled at the conclusion of the enclosing
-- 'MonadSafe' block.

buffer :: (MonadBaseControl IO m, MonadBaseControl IO (Base m),
           MonadSafe m, MonadIO m, MonadMask m)
       => Int                    -- ^ Number of slots in the bounded queue
       -> Proxy a' a () b m r    -- ^ Upstream producer
       -> Proxy () b c' c m r    -- ^ Downstream consumer
       -> Proxy a' a c' c m r
buffer sz ups downs = M $ do
    q   <- liftIO $ newTBQueueIO 3  -- control channel
    qeb <- liftIO $ newTBQueueIO sz -- bounded queue of 'b' values flowing
                                   -- from upstream to downstream
    me <- myThreadId
    hd <- spawn $ toDowns me q qeb
    hu <- spawn $ fromUps me q qeb

    mainLoop q $ \r -> do
        release hu
        release hd
        return $ Pure r
  where
    spawn f = do
        h <- async f
        link h
        register $ cancel h

    readQ q    = liftIO $ atomically $ readTBQueue q
    writeQ q x = liftIO $ atomically $ writeTBQueue q x

    mainLoop q done = loop
      where
        loop = readQ q >>= \case
            Left u -> case u of
                Request a' fa  -> return $ Request a' $ (>> M loop) . fa
                Respond _  _   -> error "Respond never comes from ups"
                M          _   -> error "M never comes from ups"
                Pure       r   -> done r
            Right d -> case d of
                Request _  _   -> error "Request never comes from downs"
                Respond c  fc' -> return $ Respond c $ (>> M loop) . fc'
                M          _   -> error "M never comes from downs"
                Pure       r   -> done r

    guarded :: (MonadBaseControl IO (Base m), MonadSafe m, MonadCatch m)
            => ThreadId
            -> ((Proxy a' a b' b m r -> m ()) -> Proxy a' a b' b m r -> m ())
            -> Proxy a' a b' b m r
            -> m ()
    guarded parent f = loop
      where
        loop p = f loop p
            `catch` (\e -> case e :: AsyncException of
                ThreadKilled -> return ()
                _ -> liftBase $ throwTo parent e)
            `catch` (\e -> liftBase $ throwTo parent (e :: SomeException))

    fromUps parent q qeb = flip (guarded parent) ups $ \loop -> \case
        Request a' fa  -> throughVar q (Left . Request a') fa >>= loop
        Respond b  fb' -> writeQ qeb (Right b) >> loop (fb' ())
        M       m      -> m >>= loop
        Pure    r      -> writeQ qeb (Left r) -- this enqueues exit

    toDowns parent q qeb = flip (guarded parent) downs $ \loop -> \case
        Request () fb  -> readQ qeb >>= \case
            Left r  -> writeQ q (Right (Pure r))
            Right b -> loop (fb b)
        Respond c  fc' -> throughVar q (Right . Respond c) fc' >>= loop
        M       m      -> m >>= loop
        Pure    r      -> writeQ q (Right (Pure r)) -- this causes exit

    throughVar q x f = do
        var <- newVar
        writeQ q $ x $ \v -> M $ do
            putVar var v
            return $ Pure $ error "(>> M loop) throws this value away"
        f `liftM` takeVar var
      where
        newVar     = liftIO newEmptyTMVarIO
        putVar v z = liftIO $ atomically $ putTMVar v z
        takeVar v  = liftIO $ atomically $ takeTMVar v

infixl 7 >&>
(>&>) :: (MonadBaseControl IO m, MonadBaseControl IO (Base m),
          MonadSafe m, MonadIO m, MonadMask m)
      => Proxy a' a () b m r -> Proxy () b c' c m r
      -> Proxy a' a c' c m r
(>&>) = buffer 16
