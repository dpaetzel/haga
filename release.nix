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
  pkgs = import <nixpkgs> { inherit config; };
in { haga = pkgs.haskellPackages.haga; }
