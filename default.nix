{ mkDerivation, base, Cabal, cassava, extra, lib, monad-loops
, MonadRandom, optparse-applicative, pipes, protolude, QuickCheck
, quickcheck-instances, random, random-fu, random-shuffle, text
, wl-pprint-text
}:
mkDerivation {
  pname = "haga";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base cassava extra monad-loops MonadRandom optparse-applicative
    pipes protolude QuickCheck quickcheck-instances random random-fu
    random-shuffle text wl-pprint-text
  ];
  executableHaskellDepends = [
    base Cabal cassava extra monad-loops MonadRandom
    optparse-applicative pipes protolude QuickCheck
    quickcheck-instances random random-fu random-shuffle text
    wl-pprint-text
  ];
  description = "Simplistic genetic algorithms library";
  license = lib.licenses.gpl3Only;
}
