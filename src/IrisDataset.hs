{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module IrisDataset
  ( module LambdaCalculus,
    module IrisDataset,
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
import qualified Language.Haskell.Interpreter as Hint
import qualified Language.Haskell.Interpreter.Unsafe as Hint
import Protolude
import qualified Type.Reflection as Ref

data IrisClass = Setosa | Virginica | Versicolor deriving (Eq, Generic, Show, Enum, Bounded)

instance FromRecord IrisClass

instance ToRecord IrisClass

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
        ( [ (5.1, 3.5, 1.4, 0.2),
            (4.9, 3.0, 1.4, 0.2),
            (4.7, 3.2, 1.3, 0.2),
            (4.6, 3.1, 1.5, 0.2),
            (5.0, 3.6, 1.4, 0.2),
            (5.4, 3.9, 1.7, 0.4),
            (4.6, 3.4, 1.4, 0.3),
            (5.0, 3.4, 1.5, 0.2),
            (4.4, 2.9, 1.4, 0.2),
            (4.9, 3.1, 1.5, 0.1),
            (5.4, 3.7, 1.5, 0.2),
            (4.8, 3.4, 1.6, 0.2),
            (4.8, 3.0, 1.4, 0.1),
            (4.3, 3.0, 1.1, 0.1),
            (5.8, 4.0, 1.2, 0.2),
            (5.7, 4.4, 1.5, 0.4),
            (5.4, 3.9, 1.3, 0.4),
            (5.1, 3.5, 1.4, 0.3),
            (5.7, 3.8, 1.7, 0.3),
            (5.1, 3.8, 1.5, 0.3),
            (5.4, 3.4, 1.7, 0.2),
            (5.1, 3.7, 1.5, 0.4),
            (4.6, 3.6, 1.0, 0.2),
            (5.1, 3.3, 1.7, 0.5),
            (4.8, 3.4, 1.9, 0.2),
            (5.0, 3.0, 1.6, 0.2),
            (5.0, 3.4, 1.6, 0.4),
            (5.2, 3.5, 1.5, 0.2),
            (5.2, 3.4, 1.4, 0.2),
            (4.7, 3.2, 1.6, 0.2),
            (4.8, 3.1, 1.6, 0.2),
            (5.4, 3.4, 1.5, 0.4),
            (5.2, 4.1, 1.5, 0.1),
            (5.5, 4.2, 1.4, 0.2),
            (4.9, 3.1, 1.5, 0.1),
            (5.0, 3.2, 1.2, 0.2),
            (5.5, 3.5, 1.3, 0.2),
            (4.9, 3.1, 1.5, 0.1),
            (4.4, 3.0, 1.3, 0.2),
            (5.1, 3.4, 1.5, 0.2),
            (5.0, 3.5, 1.3, 0.3),
            (4.5, 2.3, 1.3, 0.3),
            (4.4, 3.2, 1.3, 0.2),
            (5.0, 3.5, 1.6, 0.6),
            (5.1, 3.8, 1.9, 0.4),
            (4.8, 3.0, 1.4, 0.3),
            (5.1, 3.8, 1.6, 0.2),
            (4.6, 3.2, 1.4, 0.2),
            (5.3, 3.7, 1.5, 0.2),
            (5.0, 3.3, 1.4, 0.2),
            (7.0, 3.2, 4.7, 1.4),
            (6.4, 3.2, 4.5, 1.5),
            (6.9, 3.1, 4.9, 1.5),
            (5.5, 2.3, 4.0, 1.3),
            (6.5, 2.8, 4.6, 1.5),
            (5.7, 2.8, 4.5, 1.3),
            (6.3, 3.3, 4.7, 1.6),
            (4.9, 2.4, 3.3, 1.0),
            (6.6, 2.9, 4.6, 1.3),
            (5.2, 2.7, 3.9, 1.4),
            (5.0, 2.0, 3.5, 1.0),
            (5.9, 3.0, 4.2, 1.5),
            (6.0, 2.2, 4.0, 1.0),
            (6.1, 2.9, 4.7, 1.4),
            (5.6, 2.9, 3.6, 1.3),
            (6.7, 3.1, 4.4, 1.4),
            (5.6, 3.0, 4.5, 1.5),
            (5.8, 2.7, 4.1, 1.0),
            (6.2, 2.2, 4.5, 1.5),
            (5.6, 2.5, 3.9, 1.1),
            (5.9, 3.2, 4.8, 1.8),
            (6.1, 2.8, 4.0, 1.3),
            (6.3, 2.5, 4.9, 1.5),
            (6.1, 2.8, 4.7, 1.2),
            (6.4, 2.9, 4.3, 1.3),
            (6.6, 3.0, 4.4, 1.4),
            (6.8, 2.8, 4.8, 1.4),
            (6.7, 3.0, 5.0, 1.7),
            (6.0, 2.9, 4.5, 1.5),
            (5.7, 2.6, 3.5, 1.0),
            (5.5, 2.4, 3.8, 1.1),
            (5.5, 2.4, 3.7, 1.0),
            (5.8, 2.7, 3.9, 1.2),
            (6.0, 2.7, 5.1, 1.6),
            (5.4, 3.0, 4.5, 1.5),
            (6.0, 3.4, 4.5, 1.6),
            (6.7, 3.1, 4.7, 1.5),
            (6.3, 2.3, 4.4, 1.3),
            (5.6, 3.0, 4.1, 1.3),
            (5.5, 2.5, 4.0, 1.3),
            (5.5, 2.6, 4.4, 1.2),
            (6.1, 3.0, 4.6, 1.4),
            (5.8, 2.6, 4.0, 1.2),
            (5.0, 2.3, 3.3, 1.0),
            (5.6, 2.7, 4.2, 1.3),
            (5.7, 3.0, 4.2, 1.2),
            (5.7, 2.9, 4.2, 1.3),
            (6.2, 2.9, 4.3, 1.3),
            (5.1, 2.5, 3.0, 1.1),
            (5.7, 2.8, 4.1, 1.3),
            (6.3, 3.3, 6.0, 2.5),
            (5.8, 2.7, 5.1, 1.9),
            (7.1, 3.0, 5.9, 2.1),
            (6.3, 2.9, 5.6, 1.8),
            (6.5, 3.0, 5.8, 2.2),
            (7.6, 3.0, 6.6, 2.1),
            (4.9, 2.5, 4.5, 1.7),
            (7.3, 2.9, 6.3, 1.8),
            (6.7, 2.5, 5.8, 1.8),
            (7.2, 3.6, 6.1, 2.5),
            (6.5, 3.2, 5.1, 2.0),
            (6.4, 2.7, 5.3, 1.9),
            (6.8, 3.0, 5.5, 2.1),
            (5.7, 2.5, 5.0, 2.0),
            (5.8, 2.8, 5.1, 2.4),
            (6.4, 3.2, 5.3, 2.3),
            (6.5, 3.0, 5.5, 1.8),
            (7.7, 3.8, 6.7, 2.2),
            (7.7, 2.6, 6.9, 2.3),
            (6.0, 2.2, 5.0, 1.5),
            (6.9, 3.2, 5.7, 2.3),
            (5.6, 2.8, 4.9, 2.0),
            (7.7, 2.8, 6.7, 2.0),
            (6.3, 2.7, 4.9, 1.8),
            (6.7, 3.3, 5.7, 2.1),
            (7.2, 3.2, 6.0, 1.8),
            (6.2, 2.8, 4.8, 1.8),
            (6.1, 3.0, 4.9, 1.8),
            (6.4, 2.8, 5.6, 2.1),
            (7.2, 3.0, 5.8, 1.6),
            (7.4, 2.8, 6.1, 1.9),
            (7.9, 3.8, 6.4, 2.0),
            (6.4, 2.8, 5.6, 2.2),
            (6.3, 2.8, 5.1, 1.5),
            (6.1, 2.6, 5.6, 1.4),
            (7.7, 3.0, 6.1, 2.3),
            (6.3, 3.4, 5.6, 2.4),
            (6.4, 3.1, 5.5, 1.8),
            (6.0, 3.0, 4.8, 1.8),
            (6.9, 3.1, 5.4, 2.1),
            (6.7, 3.1, 5.6, 2.4),
            (6.9, 3.1, 5.1, 2.3),
            (5.8, 2.7, 5.1, 1.9),
            (6.8, 3.2, 5.9, 2.3),
            (6.7, 3.3, 5.7, 2.5),
            (6.7, 3.0, 5.2, 2.3),
            (6.3, 2.5, 5.0, 1.9),
            (6.5, 3.0, 5.2, 2.0),
            (6.2, 3.4, 5.4, 2.3),
            (5.9, 3.0, 5.1, 1.8)
          ],
          [ Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Setosa,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Versicolor,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica,
            Virginica
          ]
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
    results :: Map TypeRequester R
  }

instance Evaluator TypeRequester LamdaExecutionEnv where
  fitness env tr = (results env) Map.! tr

  calc env pop = do
    let toAdd = NE.filter (\k -> not (Map.member k (results env))) pop
    env <- loadTrainingData env
    toInsert <- Hint.runInterpreter (evalResults env toAdd)
    let insertPair (key, val) m = Map.insert key val m
    let res = foldr insertPair (results env) (fromRight undefined toInsert)
    return env {results = res}

loadTrainingData :: LamdaExecutionEnv -> IO LamdaExecutionEnv
loadTrainingData ex@LamdaExecutionEnv {trainingData = ([], [])} = do
  csv <- B.readFile (trainingDataset ex)
  let dat = (toList $ fromRight undefined $ decode NoHeader csv) :: [(Float, Float, Float, Float)]
  csvRes <- B.readFile (trainingDatasetRes ex)
  let decodedRes = decode NoHeader csvRes
  let recsRes = (toList $ fromRight undefined decodedRes) :: [IrisClass]
  return ex {trainingData = (dat, recsRes)}
loadTrainingData lee@LamdaExecutionEnv {trainingData = (_ : _, _ : _)} = return lee
loadTrainingData lee@LamdaExecutionEnv {trainingData = (_ : _, [])} = return undefined
loadTrainingData lee@LamdaExecutionEnv {trainingData = ([], _ : _)} = return undefined

evalResults :: LamdaExecutionEnv -> [TypeRequester] -> Hint.InterpreterT IO [(TypeRequester, R)]
evalResults ex trs = mapM (evalResult ex) trs

evalResult :: LamdaExecutionEnv -> TypeRequester -> Hint.InterpreterT IO (TypeRequester, R)
evalResult ex tr = do
  Hint.setImports $ (map T.unpack (imports ex)) ++ ["Protolude"]
  Hint.unsafeSetGhcOption "-O2"
  result <- Hint.interpret (T.unpack (toLambdaExpressionS tr)) (Hint.as :: Float -> Float -> Float -> Float -> IrisClass)
  let res = map (\(a,b,c,d) -> result a b c d) (fst (trainingData ex))
  let resAndTarget = (zip (snd (trainingData ex)) res)
  let acc = (foldr (\ts s -> if ((fst ts) == (snd ts)) then s + 1 else s) 0 resAndTarget) :: R
  let biasWellDistributed = (foldr (*) 1 (map (\ty -> (foldr (\ts s -> if ((snd ts) == ty) then s + 1 else s) 1 resAndTarget)) ([minBound..maxBound] :: [IrisClass]):: [R])) ** (1/3)
  let biasSmall = exp ( - (fromIntegral (countTrsR tr)))
  let score = acc + (biasWellDistributed/5.1) + (biasSmall)
  return (tr, score)

if' :: Bool -> a -> a -> a
if' True e _ = e
if' False _ e = e
