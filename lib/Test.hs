{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified GA
import Protolude

main :: IO ()
main = do
  _ <- GA.runTests
  return ()

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y
