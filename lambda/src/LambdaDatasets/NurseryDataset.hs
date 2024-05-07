{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LambdaDatasets.NurseryDataset
  ( module LambdaCalculusV2,
    module LambdaDatasets.NurseryDataset,
    module LambdaDatasets.NurseryData,
    module GA,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Random
import Data.Random.Distribution.Uniform
import qualified Data.Text as T
import Data.Tuple.Extra
import GA
import LambdaCalculusV2
import LambdaDatasets.NurseryData
import qualified Language.Haskell.Interpreter as Hint
import qualified Language.Haskell.Interpreter.Unsafe as Hint
import Protolude
import Protolude.Error
import System.Random.MWC (createSystemRandom)
import qualified Type.Reflection as Ref
import Utils

operators :: [BoundSymbol]
operators = [ -- Math
          -- Logic
          BoundSymbol (Ref.TypeRep @(Bool -> Bool -> Bool)) (&&) (Just "(&&)"),
          BoundSymbol (Ref.TypeRep @(Bool -> Bool -> Bool)) (||) (Just "(||)"),
          -- Ordered Enums
          BoundSymbol (Ref.TypeRep @(NurseryClass -> NurseryClass -> Bool)) (>) (Just "(>)"),
          BoundSymbol (Ref.TypeRep @(NurseryClass -> NurseryClass -> Bool)) (==) (Just "(==)"),
          BoundSymbol (Ref.TypeRep @(NurseryClass -> NurseryClass -> Bool)) (/=) (Just "(/=)"),
          BoundSymbol (Ref.TypeRep @(NurseryClass -> NurseryClass -> Bool)) (>=) (Just "(>=)"),
          BoundSymbol (Ref.TypeRep @(Parents -> Parents -> Bool)) (>) (Just "(>)"),
          BoundSymbol (Ref.TypeRep @(Parents -> Parents -> Bool)) (==) (Just "(==)"),
          BoundSymbol (Ref.TypeRep @(Parents -> Parents -> Bool)) (/=) (Just "(/=)"),
          BoundSymbol (Ref.TypeRep @(Parents -> Parents -> Bool)) (>=) (Just "(>=)"),
          BoundSymbol (Ref.TypeRep @(HasNurs -> HasNurs -> Bool)) (>) (Just "(>)"),
          BoundSymbol (Ref.TypeRep @(HasNurs -> HasNurs -> Bool)) (==) (Just "(==)"),
          BoundSymbol (Ref.TypeRep @(HasNurs -> HasNurs -> Bool)) (/=) (Just "(/=)"),
          BoundSymbol (Ref.TypeRep @(HasNurs -> HasNurs -> Bool)) (>=) (Just "(>=)"),
          BoundSymbol (Ref.TypeRep @(Form -> Form -> Bool)) (>) (Just "(>)"),
          BoundSymbol (Ref.TypeRep @(Form -> Form -> Bool)) (==) (Just "(==)"),
          BoundSymbol (Ref.TypeRep @(Form -> Form -> Bool)) (/=) (Just "(/=)"),
          BoundSymbol (Ref.TypeRep @(Form -> Form -> Bool)) (>=) (Just "(>=)"),
          BoundSymbol (Ref.TypeRep @(Children -> Children -> Bool)) (>) (Just "(>)"),
          BoundSymbol (Ref.TypeRep @(Children -> Children -> Bool)) (==) (Just "(==)"),
          BoundSymbol (Ref.TypeRep @(Children -> Children -> Bool)) (/=) (Just "(/=)"),
          BoundSymbol (Ref.TypeRep @(Children -> Children -> Bool)) (>=) (Just "(>=)"),
          BoundSymbol (Ref.TypeRep @(Housing -> Housing -> Bool)) (>) (Just "(>)"),
          BoundSymbol (Ref.TypeRep @(Housing -> Housing -> Bool)) (==) (Just "(==)"),
          BoundSymbol (Ref.TypeRep @(Housing -> Housing -> Bool)) (/=) (Just "(/=)"),
          BoundSymbol (Ref.TypeRep @(Housing -> Housing -> Bool)) (>=) (Just "(>=)"),
          BoundSymbol (Ref.TypeRep @(Finance -> Finance -> Bool)) (>) (Just "(>)"),
          BoundSymbol (Ref.TypeRep @(Finance -> Finance -> Bool)) (==) (Just "(==)"),
          BoundSymbol (Ref.TypeRep @(Finance -> Finance -> Bool)) (/=) (Just "(/=)"),
          BoundSymbol (Ref.TypeRep @(Finance -> Finance -> Bool)) (>=) (Just "(>=)"),
          BoundSymbol (Ref.TypeRep @(Social -> Social -> Bool)) (>) (Just "(>)"),
          BoundSymbol (Ref.TypeRep @(Social -> Social -> Bool)) (==) (Just "(==)"),
          BoundSymbol (Ref.TypeRep @(Social -> Social -> Bool)) (/=) (Just "(/=)"),
          BoundSymbol (Ref.TypeRep @(Social -> Social -> Bool)) (>=) (Just "(>=)"),
          BoundSymbol (Ref.TypeRep @(Health -> Health -> Bool)) (>) (Just "(>)"),
          BoundSymbol (Ref.TypeRep @(Health -> Health -> Bool)) (==) (Just "(==)"),
          BoundSymbol (Ref.TypeRep @(Health -> Health -> Bool)) (/=) (Just "(/=)"),
          BoundSymbol (Ref.TypeRep @(Health -> Health -> Bool)) (>=) (Just "(>=)"),
          -- Eq Enum
          -- Any Type
          BoundSymbol (Ref.TypeRep @(Bool -> Int -> Int -> Int)) (if') (Just "if'"),
          BoundSymbol (Ref.TypeRep @(Bool -> NurseryClass -> NurseryClass -> NurseryClass)) (if') (Just "if'"),
          BoundSymbol (Ref.TypeRep @(Bool -> Parents -> Parents -> Parents)) (if') (Just "if'"),
          BoundSymbol (Ref.TypeRep @(Bool -> HasNurs -> HasNurs -> HasNurs)) (if') (Just "if'"),
          BoundSymbol (Ref.TypeRep @(Bool -> Form -> Form -> Form)) (if') (Just "if'"),
          BoundSymbol (Ref.TypeRep @(Bool -> Children -> Children -> Children)) (if') (Just "if'"),
          BoundSymbol (Ref.TypeRep @(Bool -> Housing -> Housing -> Housing)) (if') (Just "if'"),
          BoundSymbol (Ref.TypeRep @(Bool -> Finance -> Finance -> Finance)) (if') (Just "if'"),
          BoundSymbol (Ref.TypeRep @(Bool -> Social -> Social -> Social)) (if') (Just "if'"),
          BoundSymbol (Ref.TypeRep @(Bool -> Health -> Health -> Health)) (if') (Just "if'")
        ]


lE :: LambdaEnviroment (Parents -> HasNurs -> Form -> Children -> Housing -> Finance -> Social -> Health -> NurseryClass)
lE =
  LambdaEnviroment
    { functions = operators,
      constants =
          [ ConstVal (Ref.TypeRep @(Bool)) (uniform True False),
            ConstVal (Ref.TypeRep @(NurseryClass)) (enumUniform NotRecommend SpecPriority),
            ConstVal (Ref.TypeRep @(Parents)) (enumUniform Usual GreatPret),
            ConstVal (Ref.TypeRep @(HasNurs)) (enumUniform ProperNurs VeryCritNurs),
            ConstVal (Ref.TypeRep @(Form)) (enumUniform CompleteFamilyForm FosterFamilyForm),
            ConstVal (Ref.TypeRep @(Children)) (enumUniform OneChild MoreChilds),
            ConstVal (Ref.TypeRep @(Housing)) (enumUniform ConvenientHousing CriticalHousing),
            ConstVal (Ref.TypeRep @(Finance)) (enumUniform ConvenientFinance InconvFinance),
            ConstVal (Ref.TypeRep @(Social)) (enumUniform NotProblematicSocial ProblematicSocial),
            ConstVal (Ref.TypeRep @(Health)) (enumUniform NotRecommendHealth PriorityHealth)
          ],
      maxDepth = 150,
      weights =
        ExpressionWeights
          { application = 2,
            abstraction = 2,
            variableReference = 300,
            constant = 1,
            functionBias = 100
          },
      mutationStrength = 10/150,
      crossoverStrength = 15/150
    }

trainingFraction :: R
trainingFraction = (2 / 3)

lEE :: ExecutionEnviroment (Parents -> HasNurs -> Form -> Children -> Housing -> Finance -> Social -> Health -> NurseryClass)
lEE =
  ExecutionEnviroment
    { -- For now these need to define all available functions and types. Generic functions can be used.
      fun = operators,
      training = True,
      trainingData = nurseryTrainingData,
      testData = nurseryTrainingData
    }

shuffledLEE :: IO (ExecutionEnviroment (Parents -> HasNurs -> Form -> Children -> Housing -> Finance -> Social -> Health -> NurseryClass))
shuffledLEE = do
  return
    ExecutionEnviroment
      { fun = operators,
        training = True,
        trainingData = nurseryTrainingData,
        testData = nurseryTrainingData
      }
