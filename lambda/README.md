# Why this split:


The Module(s) used when evaluating individuals has to be in an external library to make Hint work. so we split the lamda-calculus command program in a library we need to expose in the main library and the implementation.

Sadly, ghc / ghci / cabal can not properly make a public, internal library available to ghci (and, with that, Hint). Should this ever change:
```
library haga-lambda-lib
  visibility:          public
  build-depends:       base
                     , protolude
  default-language:    Haskell2010
  ghc-options:         -Wall -Wno-orphans -O2
  hs-source-dirs:      lambda/lib
  other-modules:       CommonDefinition
  exposed-modules:     LambdaDatasets.NurseryDefinition
```
