{ mkDerivation, base, monad-loops, MonadRandom, protolude
, QuickCheck, quickcheck-instances, random, random-fu
, random-shuffle, stdenv, text
}:
mkDerivation {
  pname = "GA-PFP";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base monad-loops MonadRandom protolude QuickCheck
    quickcheck-instances random random-fu random-shuffle text
  ];
  license = stdenv.lib.licenses.gpl3;
}
