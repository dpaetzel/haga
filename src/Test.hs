{-# LANGUAGE NoImplicitPrelude #-}
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
import qualified Seminar
import qualified LambdaCalculus
import Data.Typeable
import qualified Type.Reflection as Ref

main :: IO ()
main = do
  _ <- GA.runTests
  _ <- Seminar.runTests
  _ <- putStrLn $ ((show (typeRepArgs (Ref.SomeTypeRep (Ref.TypeRep @(Int->Int->Int->Text))))) :: Text)
  _ <- putStrLn $ ((show (typeRepArgs (Ref.SomeTypeRep (Ref.TypeRep @(Text))))) :: Text)
  _ <- putStrLn (LambdaCalculus.toLambdaExpressionShort LambdaCalculus.testIntToClassCorrect)
  _ <- putStrLn $ ((show (LambdaCalculus.res 1)) :: Text)
  _ <- putStrLn $ ((show (LambdaCalculus.res 2)) :: Text)
  _ <- putStrLn $ ((show (LambdaCalculus.res 3)) :: Text)
  return ()
