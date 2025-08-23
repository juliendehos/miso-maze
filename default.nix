{ pkgs ? import ./nixpkgs.nix }:

let
  ghc = pkgs.haskellPackages;
  # ghc = pkgs.pkgs.haskell.packages.ghc9122;

in 
  ghc.developPackage {

    root = ./.;

    withHoogle = false;

    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with ghc; [
        miso
        cabal-install
      ]);
  }

