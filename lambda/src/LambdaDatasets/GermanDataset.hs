{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LambdaDatasets.GermanDataset
  ( module LambdaCalculus,
    module LambdaDatasets.GermanDataset,
    module LambdaDatasets.GermanData,
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
import LambdaDatasets.GermanData
import LambdaCalculus
import qualified Language.Haskell.Interpreter as Hint
import qualified Language.Haskell.Interpreter.Unsafe as Hint
import Protolude
import Protolude.Error
import System.Random.MWC (createSystemRandom)
import qualified Type.Reflection as Ref
import Utils

lE :: LambdaEnviroment
lE =
  LambdaEnviroment
    { functions =
        Map.fromList
          [ -- Math
            ((Ref.SomeTypeRep (Ref.TypeRep @(Int -> Int -> Int))), ["(+)", "(-)", "(*)"]),
            -- Logic
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> Bool -> Bool))), ["(&&)", "(||)"]),
            -- Ordered Enums
            ((Ref.SomeTypeRep (Ref.TypeRep @(Int -> Int -> Bool))), ["(>)", "(==)", "(/=)", "(>=)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(AccountStatus -> AccountStatus -> Bool))), ["(>)", "(==)", "(/=)", "(>=)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(CreditHistory -> CreditHistory -> Bool))), ["(>)", "(==)", "(/=)", "(>=)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Savings -> Savings -> Bool))), ["(>)", "(==)", "(/=)", "(>=)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(EmploymentStatus -> EmploymentStatus -> Bool))), ["(>)", "(==)", "(/=)", "(>=)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(OtherDebtors -> OtherDebtors -> Bool))), ["(>)", "(==)", "(/=)", "(>=)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Job -> Job -> Bool))), ["(>)", "(==)", "(/=)", "(>=)"]),
            -- Eq Enum
            ((Ref.SomeTypeRep (Ref.TypeRep @(GermanClass -> GermanClass -> Bool))), ["(==)", "(/=)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Purpose -> Purpose -> Bool))), ["(==)", "(/=)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(StatusAndSex -> StatusAndSex -> Bool))), ["(==)", "(/=)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Property -> Property -> Bool))), ["(==)", "(/=)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(OtherPlans -> OtherPlans -> Bool))), ["(==)", "(/=)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Housing -> Housing -> Bool))), ["(==)", "(/=)"]),
            -- Any Type
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> Int -> Int -> Int))), ["if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> GermanClass -> GermanClass -> GermanClass))), ["if'","if'","if'","if'","if'","if'","if'","if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> AccountStatus -> AccountStatus -> AccountStatus))), ["if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> CreditHistory -> CreditHistory -> CreditHistory))), ["if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> Purpose -> Purpose -> Purpose))), ["if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> Savings -> Savings -> Savings))), ["if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> EmploymentStatus -> EmploymentStatus -> EmploymentStatus))), ["if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> StatusAndSex -> StatusAndSex -> StatusAndSex))), ["if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> OtherDebtors -> OtherDebtors -> OtherDebtors))), ["if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> Property -> Property -> Property))), ["if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> OtherPlans -> OtherPlans -> OtherPlans))), ["if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> Housing -> Housing -> Housing))), ["if'"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> Job -> Job -> Job))), ["if'"])
          ],
      constants =
        Map.fromList
          [ ((Ref.SomeTypeRep (Ref.TypeRep @(Int))), [(fmap show (uniform 0 10 :: RVar Int))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool))), [(fmap show (uniform True False :: RVar Bool))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(GermanClass))), [(fmap show (enumUniform Accept Deny))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(AccountStatus))), [(fmap show (enumUniform AccountInDebt HighAccountBalanceOrRegular))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(CreditHistory))), [(fmap show (enumUniform HistoryGood CreditsExist ))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Purpose))), [(fmap show (enumUniform OldCar Other ))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Savings))), [(fmap show (enumUniform UnknownOrNone GreatSavings ))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(EmploymentStatus))), [(fmap show (enumUniform NotEmployed VeteranEmployed ))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(StatusAndSex))), [(fmap show (enumUniform MaleAndSeperated MaleAndWidowedOrMarried ))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(OtherDebtors))), [(fmap show (enumUniform NoOtherDebtors Guarantor ))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Property))), [(fmap show (enumUniform UnknownOrNoProperty CarOrOther ))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(OtherPlans))), [(fmap show (enumUniform PlansAtBank NoOtherPlans ))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Housing))), [(fmap show (enumUniform Renting ResidingForFree ))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Job))), [(fmap show (enumUniform UnemployedOrUnskilledNonResident HighlySkilled ))])
          ],
      targetType = (Ref.SomeTypeRep (Ref.TypeRep @(AccountStatus -> Int -> CreditHistory -> Purpose -> Int -> Savings -> EmploymentStatus -> Int -> StatusAndSex -> OtherDebtors -> Int -> Property -> Int -> OtherPlans -> Housing -> Int -> Job -> Int -> Bool -> Bool -> GermanClass))),
      maxDepth = 8,
      weights =
        ExpressionWeights
          { lambdaSpucker = 1,
            lambdaSchlucker = 2,
            symbol = 30,
            variable = 10,
            constant = 5
          }
    }

lEE :: LamdaExecutionEnv
lEE =
  LamdaExecutionEnv
    { -- For now these need to define all available functions and types. Generic functions can be used.
      imports = ["LambdaDatasets.GermanDefinition"],
      training = True,
      trainingData =
        ( map fst (takeFraktion 0.8 germanTrainingData),
          map snd (takeFraktion 0.8 germanTrainingData)
        ),
      testData =
        ( map fst (dropFraktion 0.8 germanTrainingData),
          map snd (dropFraktion 0.8 germanTrainingData)
        ),
      exTargetType = (Ref.SomeTypeRep (Ref.TypeRep @(AccountStatus -> Int -> CreditHistory -> Purpose -> Int -> Savings -> EmploymentStatus -> Int -> StatusAndSex -> OtherDebtors -> Int -> Property -> Int -> OtherPlans -> Housing -> Int -> Job -> Int -> Bool -> Bool -> GermanClass))),
      results = Map.empty
    }

shuffledLEE :: IO LamdaExecutionEnv
shuffledLEE = do
  mwc <- liftIO createSystemRandom
  let smpl = ((sampleFrom mwc) :: RVar a -> IO a)
  itD <- smpl $ shuffle germanTrainingData
  return
    LamdaExecutionEnv
      { -- For now these need to define all available functions and types. Generic functions can be used.
        imports = ["LambdaDatasets.GermanDefinition"],
        training = True,
        trainingData =
          ( map fst (takeFraktion 0.8 itD),
            map snd (takeFraktion 0.8 itD)
          ),
        testData =
          ( map fst (dropFraktion 0.8 itD),
            map snd (dropFraktion 0.8 itD)
          ),
        exTargetType = (Ref.SomeTypeRep (Ref.TypeRep @(AccountStatus -> Int -> CreditHistory -> Purpose -> Int -> Savings -> EmploymentStatus -> Int -> StatusAndSex -> OtherDebtors -> Int -> Property -> Int -> OtherPlans -> Housing -> Int -> Job -> Int -> Bool -> Bool -> GermanClass))),
        results = Map.empty
      }

data LamdaExecutionEnv = LamdaExecutionEnv
  { -- For now these need to define all available functions and types. Generic functions can be used.
    imports :: [Text],
    training :: Bool,
    trainingData :: ([(AccountStatus, Int, CreditHistory, Purpose, Int, Savings, EmploymentStatus, Int, StatusAndSex, OtherDebtors, Int, Property, Int, OtherPlans, Housing, Int, Job, Int, Bool, Bool)], [GermanClass]),
    testData :: ([(AccountStatus, Int, CreditHistory, Purpose, Int, Savings, EmploymentStatus, Int, StatusAndSex, OtherDebtors, Int, Property, Int, OtherPlans, Housing, Int, Job, Int, Bool, Bool)], [GermanClass]),
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

dset :: LamdaExecutionEnv -> ([(AccountStatus, Int, CreditHistory, Purpose, Int, Savings, EmploymentStatus, Int, StatusAndSex, OtherDebtors, Int, Property, Int, OtherPlans, Housing, Int, Job, Int, Bool, Bool)], [GermanClass])
dset lEE = if training lEE then trainingData lEE else testData lEE

evalResults :: LamdaExecutionEnv -> [TypeRequester] -> Hint.InterpreterT IO [(TypeRequester, FittnesRes)]
evalResults ex trs = do
  Hint.setImports $ (map T.unpack (imports ex)) ++ ["Protolude"]
  Hint.unsafeSetGhcOption "-O2"
  let arrayOfFunctionText = map toLambdaExpressionS trs
  let textOfFunctionArray = "[" <> T.intercalate "," arrayOfFunctionText <> "]"
  result <- Hint.interpret (T.unpack (textOfFunctionArray)) (Hint.as :: [AccountStatus -> Int -> CreditHistory -> Purpose -> Int -> Savings -> EmploymentStatus -> Int -> StatusAndSex -> OtherDebtors -> Int -> Property -> Int -> OtherPlans -> Housing -> Int -> Job -> Int -> Bool -> Bool -> GermanClass])
  return $ zipWith (evalResult ex) trs result


evalResult :: LamdaExecutionEnv -> TypeRequester -> (AccountStatus -> Int -> CreditHistory -> Purpose -> Int -> Savings -> EmploymentStatus -> Int -> StatusAndSex -> OtherDebtors -> Int -> Property -> Int -> OtherPlans -> Housing -> Int -> Job -> Int -> Bool -> Bool -> GermanClass) -> (TypeRequester, FittnesRes)
evalResult ex tr result = ( tr,
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
    where
    res = map (\(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) -> result a b c d e f g h i j k l m n o p q r s t) (fst (dset ex))
    resAndTarget = (zip (snd (dset ex)) res)
    acc = (foldr (\ts s -> if ((fst ts) == (snd ts)) then s + 1 else s) 0 resAndTarget) / fromIntegral (length resAndTarget)
    biasSmall = exp ((-(fromIntegral (countTrsR tr))) / 1000) -- 0 (schlecht) bis 1 (gut)
    fitness' = meanOfAccuricyPerClass resAndTarget
    score = fitness' + (biasSmall - 1)

