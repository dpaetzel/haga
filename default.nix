{ mkDerivation, base, bytestring, cassava, monad-loops, MonadRandom
, protolude, QuickCheck, quickcheck-instances, random, random-fu
, random-shuffle, stdenv, text
}:
mkDerivation {
  pname = "ga";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base cassava monad-loops MonadRandom protolude QuickCheck
    quickcheck-instances random random-fu random-shuffle text
  ];
  executableHaskellDepends = [
    base bytestring cassava monad-loops MonadRandom protolude
    QuickCheck quickcheck-instances random random-fu random-shuffle
    text
  ];
  license = stdenv.lib.licenses.gpl3;
}
