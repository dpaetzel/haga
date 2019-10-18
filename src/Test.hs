{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude
import qualified Seminar

main :: IO ()
main = do
  _ <- Seminar.runTests
  return ()
