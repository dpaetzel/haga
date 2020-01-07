{ mkDerivation, base, Cabal, extra, monad-loops, MonadRandom
, optparse-applicative, pipes, protolude, QuickCheck
, quickcheck-instances, random, random-fu, random-shuffle, stdenv
, text
}:
mkDerivation {
  pname = "haga";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base extra monad-loops MonadRandom optparse-applicative pipes
    protolude QuickCheck quickcheck-instances random random-fu
    random-shuffle text
  ];
  executableHaskellDepends = [
    base Cabal extra monad-loops MonadRandom optparse-applicative pipes
    protolude QuickCheck quickcheck-instances random random-fu
    random-shuffle text
  ];
  description = "Simplistic genetic algorithms library";
  license = stdenv.lib.licenses.gpl3;
}
