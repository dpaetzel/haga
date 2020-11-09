let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          # NOTE to enable profiling in all other libraries (to enable for
          # haxcs, add ghc-options)
          # mkDerivation = args: super.mkDerivation (args // {
          #   enableLibraryProfiling = true;
          # });

          haga = haskellPackagesNew.callPackage ./default.nix { };
        };
      };
    };
  };
  pkgs = import (builtins.fetchGit {
    name = "nixpkgs-2020-05-13";
    url = "https://github.com/NixOS/nixpkgs/";
    rev = "a29c7741a3257f9e27daf126e18f472102eaa14b";
  }) { inherit config; };
in { haga = pkgs.haskellPackages.haga; }
