{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude
import qualified Seminar
import qualified GA

main :: IO ()
main = do
  _ <- Seminar.runTests
  _ <- GA.runTests
  return ()
