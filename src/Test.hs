{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Random
import Data.Typeable
import qualified GA
import qualified LambdaCalculus
import Protolude
import qualified Seminar
import System.Random.MWC (createSystemRandom)
import qualified Type.Reflection as Ref

main :: IO ()
main = do
  --_ <- GA.runTests
  --_ <- Seminar.runTests
  --_ <- putStrLn $ ((show (typeRepArgs (Ref.SomeTypeRep (Ref.TypeRep @(Int -> Int -> Int -> Text))))) :: Text)
  --_ <- putStrLn $ ((show (typeRepArgs (Ref.SomeTypeRep (Ref.TypeRep @(Text))))) :: Text)
  mwc <- createSystemRandom
  r <- sampleFrom mwc $ LambdaCalculus.new LambdaCalculus.exampleLE
  _ <- putStrLn $ LambdaCalculus.toLambdaExpressionS $ r
  r <- sampleFrom mwc $ LambdaCalculus.new LambdaCalculus.exampleLE
  _ <- putStrLn $ LambdaCalculus.toLambdaExpressionS $ r
  --_ <- putStrLn (LambdaCalculus.toLambdaExpressionShort LambdaCalculus.testIntToClassCorrect)
  --_ <- putStrLn $ ((show (LambdaCalculus.res 1)) :: Text)
  --_ <- putStrLn $ ((show (LambdaCalculus.res 2)) :: Text)
  --_ <- putStrLn $ ((show (LambdaCalculus.res 3)) :: Text)
  return ()

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y



