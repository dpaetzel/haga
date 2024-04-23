# Haga

… is a simplistic library for implementing genetic algorithms in Haskell. While
it was originally created to randomly assign topics to students in seminars
while respecting their priorities, since then, it has become slightly more
general with the outlook of one day being a fully fledged GA library usable for
many different problems. Haga is also used as a test bed for fun ideas such as
representing improvements in the population's fitness using MIDI sound.


# Building


```
nix develop
cabal update
cabal build
```


# Running


```
nix develop
fish run
```


# Multi-threaded


Compile with `-threaded` as a GHC option. Then run with additional option `+RTS
-N`.


# Running the assignment part remotely


Use `build-remote` to build first and then `run-remote` to run it.
