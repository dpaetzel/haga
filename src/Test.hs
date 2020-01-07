{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified GA
import Protolude
import qualified Seminar

main :: IO ()
main = do
  _ <- GA.runTests
  _ <- Seminar.runTests
  return ()
