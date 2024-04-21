{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module NurseryDataset
  ( module LambdaCalculus,
    module NurseryDataset,
    module NurseryData,
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
import NurseryData
import LambdaCalculus
import qualified Language.Haskell.Interpreter as Hint
import qualified Language.Haskell.Interpreter.Unsafe as Hint
import Protolude
import Protolude.Error
import System.Random.MWC (createSystemRandom)
import qualified Type.Reflection as Ref
import Utils

nurseryLE :: LambdaEnviroment
nurseryLE =
  LambdaEnviroment
    { functions =
        Map.fromList
          [ -- Math
            -- Logic
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> Bool -> Bool))), ["(&&)", "(||)"]),
            -- Ordered Enums
            ((Ref.SomeTypeRep (Ref.TypeRep @(NurseryClass -> NurseryClass -> Bool))), ["(>)", "(==)", "(/=)", "(>=)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Parents -> Parents -> Bool))), ["(>)", "(==)", "(/=)", "(>=)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(HasNurs -> HasNurs -> Bool))), ["(>)", "(==)", "(/=)", "(>=)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Form -> Form -> Bool))), ["(>)", "(==)", "(/=)", "(>=)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Children -> Children -> Bool))), ["(>)", "(==)", "(/=)", "(>=)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Housing -> Housing -> Bool))), ["(>)", "(==)", "(/=)", "(>=)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Finance -> Finance -> Bool))), ["(>)", "(==)", "(/=)", "(>=)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Social -> Social -> Bool))), ["(>)", "(==)", "(/=)", "(>=)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Health -> Health -> Bool))), ["(>)", "(==)", "(/=)", "(>=)"]),
            -- Eq Enum
            -- Any Type
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> Int -> Int -> Int))), ["if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> NurseryClass -> NurseryClass -> NurseryClass))), ["if'","if'","if'","if'","if'","if'","if'","if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> Parents -> Parents -> Parents))), ["if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> HasNurs -> HasNurs -> HasNurs))), ["if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> Form -> Form -> Form))), ["if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> Children -> Children -> Children))), ["if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> Housing -> Housing -> Housing))), ["if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> Finance -> Finance -> Finance))), ["if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> Social -> Social -> Social))), ["if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> Health -> Health -> Health))), ["if'"])
          ],
      constants =
        Map.fromList
          [ ((Ref.SomeTypeRep (Ref.TypeRep @(Bool))), [(fmap show (uniform True False :: RVar Bool))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(NurseryClass))), [(fmap show (enumUniform NotRecommend SpecPriority))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Parents))), [(fmap show (enumUniform Usual GreatPret))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(HasNurs))), [(fmap show (enumUniform ProperNurs VeryCritNurs ))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Form))), [(fmap show (enumUniform CompleteFamilyForm FosterFamilyForm ))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Children))), [(fmap show (enumUniform OneChild MoreChilds ))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Housing))), [(fmap show (enumUniform ConvenientHousing CriticalHousing ))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Finance))), [(fmap show (enumUniform ConvenientFinance InconvFinance ))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Social))), [(fmap show (enumUniform NotProblematicSocial ProblematicSocial ))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Health))), [(fmap show (enumUniform NotRecommendHealth PriorityHealth ))])
          ],
      targetType = (Ref.SomeTypeRep (Ref.TypeRep @(Parents -> HasNurs -> Form -> Children -> Housing -> Finance -> Social -> Health -> NurseryClass))),
      maxDepth = 8,
      weights =
        ExpressionWeights
          { lambdaSpucker = 1,
            lambdaSchlucker = 1,
            symbol = 30,
            variable = 10,
            constant = 5
          }
    }

nurseryLEE :: LamdaExecutionEnv
nurseryLEE =
  LamdaExecutionEnv
    { -- For now these need to define all available functions and types. Generic functions can be used.
      imports = ["NurseryDataset"],
      training = True,
      trainingData =
        ( map fst (takeFraktion (2/3) nurseryTrainingData),
          map snd (takeFraktion (2/3) nurseryTrainingData)
        ),
      testData =
        ( map fst (dropFraktion (2/3) nurseryTrainingData),
          map snd (dropFraktion (2/3) nurseryTrainingData)
        ),
      exTargetType = (Ref.SomeTypeRep (Ref.TypeRep @(Parents -> HasNurs -> Form -> Children -> Housing -> Finance -> Social -> Health -> NurseryClass))),
      results = Map.empty
    }

shuffledNurseryLEE :: IO LamdaExecutionEnv
shuffledNurseryLEE = do
  mwc <- liftIO createSystemRandom
  let smpl = ((sampleFrom mwc) :: RVar a -> IO a)
  itD <- smpl $ shuffle nurseryTrainingData
  return
    LamdaExecutionEnv
      { -- For now these need to define all available functions and types. Generic functions can be used.
        imports = ["NurseryDataset"],
        training = True,
        trainingData =
          ( map fst (takeFraktion (2/3) itD),
            map snd (takeFraktion (2/3) itD)
          ),
        testData =
          ( map fst (dropFraktion (2/3) itD),
            map snd (dropFraktion (2/3) itD)
          ),
        exTargetType = (Ref.SomeTypeRep (Ref.TypeRep @(Parents -> HasNurs -> Form -> Children -> Housing -> Finance -> Social -> Health -> NurseryClass))),
        results = Map.empty
      }

data LamdaExecutionEnv = LamdaExecutionEnv
  { -- For now these need to define all available functions and types. Generic functions can be used.
    imports :: [Text],
    training :: Bool,
    trainingData :: ([(Parents, HasNurs, Form, Children, Housing, Finance, Social, Health)], [NurseryClass]),
    testData :: ([(Parents, HasNurs, Form, Children, Housing, Finance, Social, Health)], [NurseryClass]),
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

dset :: LamdaExecutionEnv -> ([(Parents, HasNurs, Form, Children, Housing, Finance, Social, Health)], [NurseryClass])
dset lEE = if training lEE then trainingData lEE else testData lEE

evalResults :: LamdaExecutionEnv -> [TypeRequester] -> Hint.InterpreterT IO [(TypeRequester, FittnesRes)]
evalResults ex trs = mapM (evalResult ex) trs

evalResult :: LamdaExecutionEnv -> TypeRequester -> Hint.InterpreterT IO (TypeRequester, FittnesRes)
evalResult ex tr = do
  Hint.setImports $ (map T.unpack (imports ex)) ++ ["Protolude"]
  Hint.unsafeSetGhcOption "-O2"
  result <- Hint.interpret (T.unpack (toLambdaExpressionS tr)) (Hint.as :: Parents -> HasNurs -> Form -> Children -> Housing -> Finance -> Social -> Health -> NurseryClass)
  let res = map (\(a, b, c, d, e, f, g, h) -> result a b c d e f g h) (fst (dset ex))
  let resAndTarget = (zip (snd (dset ex)) res)
  let acc = (foldr (\ts s -> if ((fst ts) == (snd ts)) then s + 1 else s) 0 resAndTarget) / fromIntegral (length resAndTarget)
  let biasSmall = exp ((-(fromIntegral (countTrsR tr))) / 1000) -- 0 (schlecht) bis 1 (gut)
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
