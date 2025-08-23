{ pkgs ? import ./nixpkgs.nix }:

let
  app = pkgs.callPackage ./default.nix {};
  entrypoint = pkgs.writeScript "entrypoint.sh" ''
    #!${pkgs.stdenv.shell}
    $@
  '';
in
  pkgs.dockerTools.buildLayeredImage {
    name = "miso-maze";
    tag = "latest";
    created = "now";
    config = {
      Entrypoint = [ entrypoint ];
      Cmd = [ "${app}/bin/app" ];
    };
  }

