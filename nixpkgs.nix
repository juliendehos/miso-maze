 
let

  miso-src = fetchTarball {
    url = https://github.com/dmjio/miso/archive/5a47e2f15adcc453e691309d1665717f07b79d01.tar.gz;
    sha256 = "sha256:1z5p3704527xf0gzfly3j4z4vclak6kza08yxbhxkf01rq4c45vg";
    # should match cabal.project
  };
  
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: rec {
          miso = self.callCabal2nix "miso" miso-src {};
        };
      };
    };
  };

  channel = <nixpkgs>;
  # channel = fetchTarball "https://github.com/NixOS/nixpkgs/archive/25.05.tar.gz";

  pkgs = import channel { inherit config; };

in pkgs

