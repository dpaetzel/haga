{
  description = "Flake for haga";
  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/53a2c32bc66f5ae41a28d7a9a49d321172af621e";

  };

  outputs = inputs@{ self, nixpkgs }:
    with import nixpkgs {
      system = "x86_64-linux";
    }; {
      # defaultPackage.${system} = haskellPackages.callPackage ./default.nix { };
      devShell.${system} = mkShell {
        buildInputs = [
          haskell.compiler.ghc981
          git
          gcc
          gmp
          feedgnuplot
          stack
        ];
      };
    };
}
