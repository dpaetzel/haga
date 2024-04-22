{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude
import qualified Seminar

main :: IO ()
main = do
  _ <- Seminar.runTests
  return ()

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y
