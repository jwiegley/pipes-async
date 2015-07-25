{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, hspec, lifted-async, monad-control
      , pipes, pipes-safe, stdenv, stm
      }:
      mkDerivation {
        pname = "pipes-async";
        version = "0.1.0.0";
        src = ./.;
        buildDepends = [
          base lifted-async monad-control pipes pipes-safe stm
        ];
        testDepends = [
          base hspec lifted-async monad-control pipes pipes-safe stm
        ];
        homepage = "https://github.com/jwiegley/pipes-async";
        description = "A higher-level interface to using concurrency with pipes";
        license = stdenv.lib.licenses.bsd3;
      };

  drv = pkgs.haskell7101Packages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
