{
  description = "Flake for haga";
  inputs = {
    nixpkgs.url =
      # 2022-06-22
      "github:NixOS/nixpkgs/0d68d7c857fe301d49cdcd56130e0beea4ecd5aa";

  };

  outputs = inputs@{ self, nixpkgs }:
    with import nixpkgs {
      system = "x86_64-linux";
    }; {
      # defaultPackage.${system} = haskellPackages.callPackage ./default.nix { };
      devShell.${system} = mkShell {
        buildInputs = [
          feedgnuplot
          haskellPackages.cabal-install
          haskellPackages.ormolu
          haskell.compiler.ghc8107
          # TODO Switch to merljoha setup (stack and more recent ghc)
          # haskell.compiler.ghc981
          # stack
        ];
      };
    };
}
