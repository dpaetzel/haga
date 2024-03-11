{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module IrisDataset
  ( module LambdaCalculus,
    module IrisDataset,
    module IrisData,
    module GA,
  )
where

import qualified Data.ByteString.Lazy as B
import Data.Csv
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Random
import Data.Random.Distribution.Uniform
import qualified Data.Text as T
import Data.Tuple.Extra
import qualified Debug.Trace as DB
import GA
import LambdaCalculus
import IrisData
import qualified Language.Haskell.Interpreter as Hint
import qualified Language.Haskell.Interpreter.Unsafe as Hint
import Protolude
import qualified Type.Reflection as Ref

irisLE :: LambdaEnviroment
irisLE =
  LambdaEnviroment
    { functions =
        Map.fromList
          [ ((Ref.SomeTypeRep (Ref.TypeRep @(Float -> Float -> Float))), ["(+)", "(-)", "(*)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Float -> Float -> Bool))), ["(>)", "(==)", "(>=)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(IrisClass -> IrisClass -> Bool))), ["(==)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> Float -> Float -> Float))), ["if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> Bool -> Bool))), ["(&&)", "(||)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> IrisClass -> IrisClass -> IrisClass))), ["if'"])
          ],
      constants =
        Map.fromList
          [ ((Ref.SomeTypeRep (Ref.TypeRep @(Float))), [(fmap show (uniform 0 10 :: RVar Float))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool))), [(fmap show (uniform True False :: RVar Bool))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(IrisClass))), [(fmap show (enumUniform Setosa Versicolor :: RVar IrisClass))])
          ],
      targetType = (Ref.SomeTypeRep (Ref.TypeRep @(Float -> Float -> Float -> Float -> IrisClass))),
      maxDepth = 10,
      weights =
        ExpressionWeights
          { lambdaSpucker = 1,
            lambdaSchlucker = 1,
            symbol = 1,
            variable = 2,
            constant = 1
          }
    }

irisLEE :: LamdaExecutionEnv
irisLEE =
  LamdaExecutionEnv
    { -- For now these need to define all available functions and types. Generic functions can be used.
      imports = ["IrisDataset"],
      -- Path to a CSV file containing the training dataset
      trainingDataset = "./iris.csv",
      -- Path to a CSV file containing the dataset results
      trainingDatasetRes = "./res.csv",
      trainingData =
        ( map fst irisTrainingData,
          map snd irisTrainingData
        ),
      exTargetType = (Ref.SomeTypeRep (Ref.TypeRep @(Float -> Float -> Float -> Float -> IrisClass))),
      -- todo: kindaHacky
      results = Map.empty
    }

data LamdaExecutionEnv = LamdaExecutionEnv
  { -- For now these need to define all available functions and types. Generic functions can be used.
    imports :: [Text],
    -- Path to a CSV file containing the training dataset
    trainingDataset :: FilePath,
    -- Path to a CSV file containing the dataset results
    trainingDatasetRes :: FilePath,
    trainingData :: ([(Float, Float, Float, Float)], [IrisClass]),
    exTargetType :: TypeRep,
    -- todo: kindaHacky
    results :: Map TypeRequester FittnesRes
  }

data FittnesRes = FittnesRes
  { total :: R,
    fitnessTotal :: R,
    fitnessGeoMean :: R,
    fitnessMean :: R,
    accuracy :: Int,
    biasDist :: R,
    biasSize :: R
  }
  deriving (Show)

instance Fitness FittnesRes where
  getR = total

instance Evaluator TypeRequester LamdaExecutionEnv FittnesRes where
  fitness' env tr = (results env) Map.! tr

  calc env pop = do
    let toAdd = NE.filter (\k -> not (Map.member k (results env))) pop
    toInsert <- Hint.runInterpreter (evalResults env toAdd)
    let insertPair (key, val) m = Map.insert key val m
    let res = foldr insertPair (results env) (fromRight undefined toInsert)
    return env {results = res}


evalResults :: LamdaExecutionEnv -> [TypeRequester] -> Hint.InterpreterT IO [(TypeRequester, FittnesRes)]
evalResults ex trs = mapM (evalResult ex) trs

evalResult :: LamdaExecutionEnv -> TypeRequester -> Hint.InterpreterT IO (TypeRequester, FittnesRes)
evalResult ex tr = do
  Hint.setImports $ (map T.unpack (imports ex)) ++ ["Protolude"]
  Hint.unsafeSetGhcOption "-O2"
  result <- Hint.interpret (T.unpack (toLambdaExpressionS tr)) (Hint.as :: Float -> Float -> Float -> Float -> IrisClass)
  let res = map (\(a, b, c, d) -> result a b c d) (fst (trainingData ex))
  let resAndTarget = (zip (snd (trainingData ex)) res)
  let acc = (foldr (\ts s -> if ((fst ts) == (snd ts)) then s + 1 else s) 0 resAndTarget) :: Int
  let biasWellDistributed = (foldr (*) 1 (map (\ty -> (foldr (\ts s -> if ((snd ts) == ty) then s + 1 else s) 1 resAndTarget)) ([minBound .. maxBound] :: [IrisClass]) :: [R])) ** (1 / 3) -- 1 (schlecht) bis 51 (gut)
  let biasSmall = exp (-(fromIntegral (countTrsR tr))) -- 0 (schlecht) bis 1 (gut)
  let fitness' = meanOfAccuricyPerClass resAndTarget
  let score = fitness' + (biasSmall - 1)
  return
    ( tr,
      FittnesRes
        { total = score,
          fitnessTotal = fitness',
          fitnessMean = meanOfAccuricyPerClass resAndTarget,
          fitnessGeoMean = geomeanOfDistributionAccuracy resAndTarget,
          accuracy = acc,
          biasDist = biasWellDistributed,
          biasSize = biasSmall
        }
    )

if' :: Bool -> a -> a -> a
if' True e _ = e
if' False _ e = e
