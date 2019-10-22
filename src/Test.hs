{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified GA
import Protolude
import qualified Seminar

main :: IO ()
main = do
  _ <- Seminar.runTests
  _ <- GA.runTests
  return ()
