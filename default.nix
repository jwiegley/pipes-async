{ mkDerivation, base, lifted-async, monad-control, pipes
, pipes-safe, stdenv, stm, hspec, transformers-base
}:
mkDerivation {
  pname = "pipes-async";
  version = "0.1.1";
  src = ./.;
  buildDepends = [
    base lifted-async monad-control pipes pipes-safe stm transformers-base
  ];
  testDepends = [ hspec ];
  homepage = "https://github.com/jwiegley/pipes-async";
  description = "A higher level interface to using concurrency with pipes";
  license = stdenv.lib.licenses.bsd3;
}
