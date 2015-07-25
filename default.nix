{ mkDerivation, base, lifted-async, monad-control, pipes
, pipes-safe, stdenv, stm, hspec
}:
mkDerivation {
  pname = "pipes-async";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    base lifted-async monad-control pipes pipes-safe stm
  ];
  testDepends = [ hspec ];
  homepage = "https://github.com/jwiegley/pipes-async";
  description = "A higher level interface to using concurrency with pipes";
  license = stdenv.lib.licenses.bsd3;
}
