let
  pkgs = import <nixpkgs> { };
in
  { ga = pkgs.haskellPackages.callPackage ./default.nix { };
  }
