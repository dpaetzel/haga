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

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Random
import System.Random.MWC (createSystemRandom)
import Data.Random.Distribution.Uniform
import qualified Data.Text as T
import Data.Tuple.Extra
import GA
import LambdaCalculus
import IrisData
import qualified Language.Haskell.Interpreter as Hint
import qualified Language.Haskell.Interpreter.Unsafe as Hint
import Protolude
import Utils
import Protolude.Error
import qualified Type.Reflection as Ref

irisLE :: LambdaEnviroment
irisLE =
  LambdaEnviroment
    { functions =
        Map.fromList
          [ ((Ref.SomeTypeRep (Ref.TypeRep @(Float -> Float -> Float))), ["(+)", "(-)", "(*)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Float -> Float -> Bool))), ["(>)", "(==)", "(>=)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(IrisClass -> IrisClass -> Bool))), ["(==)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> Float -> Float -> Float))), ["if'","if'","if'","if'","if'","if'","if'","if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> Bool -> Bool))), ["(&&)", "(||)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> IrisClass -> IrisClass -> IrisClass))), ["if'","if'","if'","if'","if'","if'","if'","if'","if'","if'"])
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
            symbol = 30,
            variable = 100,
            constant = 5
          }
    }

irisLEE :: LamdaExecutionEnv
irisLEE =
  LamdaExecutionEnv
    { -- For now these need to define all available functions and types. Generic functions can be used.
      imports = ["IrisDataset"],
      training = True,
      trainingData =
        ( map fst (takeFraktion 0.8 irisTrainingData),
          map snd (takeFraktion 0.8 irisTrainingData)
        ),
      testData =
        ( map fst (dropFraktion 0.8 irisTrainingData),
          map snd (dropFraktion 0.8 irisTrainingData)
        ),
      exTargetType = (Ref.SomeTypeRep (Ref.TypeRep @(Float -> Float -> Float -> Float -> IrisClass))),
      results = Map.empty
    }

shuffledIrisLEE :: IO LamdaExecutionEnv
shuffledIrisLEE = do
  mwc <- liftIO createSystemRandom
  let smpl = ((sampleFrom mwc) :: RVar a -> IO a)
  itD <- smpl $ shuffle irisTrainingData
  return  LamdaExecutionEnv
    { -- For now these need to define all available functions and types. Generic functions can be used.
      imports = ["IrisDataset"],
      training = True,
      trainingData =
        ( map fst (takeFraktion 0.8 itD),
          map snd (takeFraktion 0.8 itD)
        ),
      testData =
        ( map fst (dropFraktion 0.8 itD),
          map snd (dropFraktion 0.8 itD)
        ),
      exTargetType = (Ref.SomeTypeRep (Ref.TypeRep @(Float -> Float -> Float -> Float -> IrisClass))),
      results = Map.empty
    }

data LamdaExecutionEnv = LamdaExecutionEnv
  { -- For now these need to define all available functions and types. Generic functions can be used.
    imports :: [Text],
    training :: Bool,
    trainingData :: ([(Float, Float, Float, Float)], [IrisClass]),
    testData :: ([(Float, Float, Float, Float)], [IrisClass]),
    exTargetType :: TypeRep,
    -- todo: kindaHacky
    results :: Map TypeRequester FittnesRes
  }

data FittnesRes = FittnesRes
  { total :: R,
    fitnessTotal :: R,
    fitnessGeoMean :: R,
    fitnessMean :: R,
    accuracy :: R,
    biasSize :: R,
    totalSize :: N
  }
  deriving (Show)

instance Fitness FittnesRes where
  getR = total

instance Evaluator TypeRequester LamdaExecutionEnv FittnesRes where
  fitness' env tr = (results env) Map.! tr

  calc env pop = do
    let relevantResults = Map.filterWithKey (\k _ -> contains pop k) (results env)
    let toAdd = NE.filter (\k -> not (Map.member k relevantResults)) pop
    toInsert <- Hint.runInterpreter (evalResults env toAdd)
    let insertPair (key, val) m = Map.insert key val m
    let res = foldr insertPair relevantResults (fromRight (error ("To insert is " <> show toInsert)) toInsert)
    return env {results = res}

dset :: LamdaExecutionEnv -> ([(Float, Float, Float, Float)], [IrisClass])
dset lEE = if training lEE then trainingData lEE else testData lEE

evalResults :: LamdaExecutionEnv -> [TypeRequester] -> Hint.InterpreterT IO [(TypeRequester, FittnesRes)]
evalResults ex trs = mapM (evalResult ex) trs

evalResult :: LamdaExecutionEnv -> TypeRequester -> Hint.InterpreterT IO (TypeRequester, FittnesRes)
evalResult ex tr = do
  Hint.setImports $ (map T.unpack (imports ex)) ++ ["Protolude"]
  Hint.unsafeSetGhcOption "-O2"
  result <- Hint.interpret (T.unpack (toLambdaExpressionS tr)) (Hint.as :: Float -> Float -> Float -> Float -> IrisClass)
  let res = map (\(a, b, c, d) -> result a b c d) (fst (dset ex))
  let resAndTarget = (zip (snd (dset ex)) res)
  let acc =  (foldr (\ts s -> if ((fst ts) == (snd ts)) then s + 1 else s) 0 resAndTarget) / fromIntegral (length resAndTarget)
  let biasSmall = exp ((-(fromIntegral (countTrsR tr)))/1000) -- 0 (schlecht) bis 1 (gut)
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
          biasSize = biasSmall,
          totalSize = countTrsR tr
        }
    )

if' :: Bool -> a -> a -> a
if' True e _ = e
if' False _ e = e

