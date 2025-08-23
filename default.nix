{ pkgs ? import <nixpkgs> {} }:

let
  # ghc = pkgs.pkgs.haskell.packages.ghc9122;
  ghc = pkgs.haskellPackages;

in 
  ghc.developPackage {

    root = ./.;

    withHoogle = false;

    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with ghc; [
        cabal-install
      ]);
  }

