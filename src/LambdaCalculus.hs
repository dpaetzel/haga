{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}


module LambdaCalculus where

import Data.Dynamic
import Data.List (foldr1, last, lookup, zipWith3, (!!), (\\))
import Data.List.Extra (delete, nubOrd, nubOrdOn)
import Data.Tuple.Extra
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Random
import qualified Data.ByteString.Lazy as B
import Data.Csv
import Data.Proxy
import qualified Data.Text as T
import Data.Typeable
import GA
import Pretty
import Protolude
import Test.QuickCheck hiding (sample, shuffle)
import Test.QuickCheck.Monadic (assert, monadicIO)
import qualified Type.Reflection as Ref
import qualified Language.Haskell.Interpreter as Hint

data ExpressionWeights = ExpressionWeights
  { lambdaSpucker :: Int,
    lambdaSchlucker :: Int,
    symbol :: Int,
    variable :: Int,
    constant :: Int
  }

data LambdaEnviroment = LambdaEnviroment
  { functions :: (Map TypeRep [ConVal]),
    constants :: (Map TypeRep [RVar ConVal]),
    targetType :: TypeRep,
    maxDepth :: Int,
    weights :: ExpressionWeights
  }

data LamdaExecutionEnv = LamdaExecutionEnv {
    -- For now these need to define all available functions and types. Generic functions can be used.
    imports :: [Text],
    --Path to a CSV file containing the training dataset
    trainingDataset :: FilePath,
    --Path to a CSV file containing the dataset results
    trainingDatasetRes :: FilePath,
    exTargetType :: TypeRep,
    -- todo: kindaHacky
    results :: Map TypeRequester R
  }

showSanifid:: Show a => a -> Text
showSanifid var = T.replace " -> " "To" (show var)

exampleLE :: LambdaEnviroment
exampleLE =
  LambdaEnviroment
    { functions =
        Map.fromList
          [ ((Ref.SomeTypeRep (Ref.TypeRep @(Int -> Int -> Int))), ["(+)", "(-)", "(*)", "mod"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Int -> Int -> Bool))), ["(>)", "(==)", "(>=)"]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool -> Int -> Int -> Int))), ["if'"])
          ],
      constants =
        Map.fromList
          [ ((Ref.SomeTypeRep (Ref.TypeRep @(Int))), [(fmap show (uniform 0 10000 :: RVar Int))]),
            ((Ref.SomeTypeRep (Ref.TypeRep @(Bool))), [(fmap show (uniform True False :: RVar Bool))])
          ],
      targetType = (Ref.SomeTypeRep (Ref.TypeRep @(Int -> Int -> Int))),
      maxDepth = 10,
      weights =
        ExpressionWeights
          { lambdaSpucker = 1,
            lambdaSchlucker = 1,
            symbol = 1,
            variable = 1,
            constant = 1
          }
    }

type BoundVars = [TypeRep]

-- we need a dynamic typ with a concept of equality here, should we want to interpret the result, instead of compiling it...
type ConVal = Text

-- LambdaSpucker - adds TypeRequester#1 as bound var and returns the result of TypeRequester#2

data LambdaExpression = LambdaSpucker TypeRequester TypeRequester BoundVars | LambdaSchlucker TypeRequester BoundVars | Symbol ConVal [TypeRequester] BoundVars | Var TypeRep Int [TypeRequester] BoundVars | Constan ConVal deriving (Eq, Ord)

asList :: LambdaExpression -> [TypeRequester]
asList (LambdaSpucker tr1 tr2 _) = [tr1, tr2]
asList (LambdaSchlucker tr _) = [tr]
asList (Symbol _ trs _) = trs
asList (Var _ _ trs _) = trs
asList (Constan _) = []



data TypeRequester = TR TypeRep (Maybe LambdaExpression) BoundVars deriving (Eq, Ord)

toLambdaExpressionS :: TypeRequester -> Text
toLambdaExpressionS (TR typeRep (Just lambdaExpression) boundVars) = "((" <> eToLambdaExpressionS lambdaExpression <> ") :: (" <> show typeRep <> "))"
toLambdaExpressionS (TR _ (Nothing) _) = "Invalid Lambda Epr"

-- data LambdaExpression = LambdaSpucker TypeRequester TypeRequester BoundVars | LambdaSchlucker TypeRequester BoundVars | Symbol ConVal [TypeRequester] BoundVars | Var TypeRep Int | Constan ConVal

eToLambdaExpressionS :: LambdaExpression -> Text
eToLambdaExpressionS (LambdaSpucker typeRequester1 typeRequester2 boundVars) = "(\\l" <> showSanifid (last boundVars) <> show (count boundVars (last boundVars) - 1) <> " -> " <> toLambdaExpressionS typeRequester2 <> ") " <> toLambdaExpressionS typeRequester1
eToLambdaExpressionS (LambdaSchlucker typeRequester boundVars) = "\\l" <> showSanifid (last boundVars) <> show (count boundVars (last boundVars) - 1) <> " -> " <> toLambdaExpressionS typeRequester
eToLambdaExpressionS (Symbol (valS) typeRequesters _) = valS <> " " <> (unwords (map toLambdaExpressionS typeRequesters))
eToLambdaExpressionS (Var typeRep int typeRequesters _) = "l" <> showSanifid typeRep <> show int <> " " <> (unwords (map toLambdaExpressionS typeRequesters))
eToLambdaExpressionS (Constan (valS)) = valS

instance Pretty TypeRequester where
  pretty = toLambdaExpressionShort

instance Individual TypeRequester

instance Pretty LambdaEnviroment where
  pretty (LambdaEnviroment functions constants target _ _) = "Functions: " <> show functions <> " Constants: " <> show (Map.keys constants) <> " Target is a function: " <> show target

genTypeRequester :: LambdaEnviroment -> Int -> TypeRep -> BoundVars -> RVar TypeRequester
genTypeRequester env depthLeft target boundVars = do
  le <- genLambdaExpression env (depthLeft - 1) target boundVars
  return (TR target (Just le) boundVars)

genLambdaExpression :: LambdaEnviroment -> Int -> TypeRep -> BoundVars -> RVar LambdaExpression
genLambdaExpression env@(LambdaEnviroment functions constants _ _ weights) depthLeft target boundVar = do
  let weightMap =
        ( if not (canGenSchlucker target)
            then [(constant weights, genLambdaConst env depthLeft target boundVar)]
            else []
        )
          <> ( if depthLeft > 0
                 then [(lambdaSpucker weights, genLambdaSpucker env depthLeft target boundVar)]
                 else []
             )
          <> ( if canGenSchlucker target
                 then [(lambdaSchlucker weights, genLambdaSchlucker env depthLeft target boundVar)]
                 else []
             )
          <> ( if depthLeft > 0 && doAnyMatchThatType target (Map.keys functions)
                 then [(symbol weights, genLambdaSymbol env depthLeft target boundVar)]
                 else []
             )
          <> ( if depthLeft > 0 && doAnyMatchThatType target boundVar
                 then [(variable weights, genLambdaVar env depthLeft target boundVar)]
                 else []
             )
  expres <- selectWeighted weightMap
  res <- expres
  return res

selectWeighted :: [(Int, a)] -> RVar a
selectWeighted x = do
  let total = sum (map fst x)
  selection <- uniform 1 total
  return $ selectAtWeight selection (NE.fromList x)

selectAtWeight :: Int -> NonEmpty (Int, a) -> a
selectAtWeight _ (x :| []) = snd x
selectAtWeight w (x :| xs)
  | fst x >= w = snd x
  | otherwise = selectAtWeight (w - fst x) (NE.fromList xs)

canGenSchlucker :: TypeRep -> Bool
canGenSchlucker t = (typeRepTyCon t) == (typeRepTyCon (Ref.SomeTypeRep (Ref.TypeRep @(->))))

doAnyMatchThatType :: TypeRep -> [TypeRep] -> Bool
doAnyMatchThatType toGen available = any (doTypesMatch toGen) available

doTypesMatch :: TypeRep -> TypeRep -> Bool
doTypesMatch toGen available = elem toGen (available : (repeatedly (lastMay . typeRepArgs) available))

genLambdaSpucker :: LambdaEnviroment -> Int -> TypeRep -> BoundVars -> RVar LambdaExpression
genLambdaSpucker env@(LambdaEnviroment functions constants _ _ weights) depthLeft target boundVar = do
  lamdaTypeLength <- uniform 1 3
  lambaTypes <- replicateM lamdaTypeLength (randomElement (Map.keys constants))
  let lambaType = foldr1 mkFunTy lambaTypes
  lamdaVarTypeRequester <- genTypeRequester env depthLeft lambaType boundVar
  typeRequester <- genTypeRequester env depthLeft target (boundVar ++ [lambaType])
  return (LambdaSpucker lamdaVarTypeRequester typeRequester (boundVar ++ [lambaType]))

genLambdaSchlucker :: LambdaEnviroment -> Int -> TypeRep -> BoundVars -> RVar LambdaExpression
genLambdaSchlucker env@(LambdaEnviroment functions constants _ _ weights) depthLeft target boundVar = do
  let args = typeRepArgs target
  let lambaType = fromJust (head args)
  let toFind = last args
  typeRequester <- genTypeRequester env depthLeft toFind (boundVar ++ [lambaType])
  return (LambdaSchlucker typeRequester (boundVar ++ [lambaType]))

genLambdaConst :: LambdaEnviroment -> Int -> TypeRep -> BoundVars -> RVar LambdaExpression
genLambdaConst env@(LambdaEnviroment functions constants _ _ weights) depthLeft target boundVar = do
  elm <- randomElement $ fromJust (Map.lookup target constants)
  res <- elm
  return $ Constan res

genLambdaSymbol :: LambdaEnviroment -> Int -> TypeRep -> BoundVars -> RVar LambdaExpression
genLambdaSymbol env@(LambdaEnviroment functions constants _ _ weights) depthLeft target boundVar = do
  let availFunTypes = filter (doTypesMatch target) (Map.keys functions)
  (tr, fun) <- randomElement $ concatMap (\l -> zip (repeat l) (fromMaybe [] (Map.lookup l functions))) availFunTypes
  ret <- genLambdaSymbol' tr fun [] env depthLeft target boundVar
  return ret

genLambdaSymbol' :: TypeRep -> ConVal -> [TypeRequester] -> LambdaEnviroment -> Int -> TypeRep -> BoundVars -> RVar LambdaExpression
genLambdaSymbol' tr v trs env@(LambdaEnviroment functions constants _ _ weights) depthLeft target boundVar
  | tr == target = do
      return $ Symbol v trs boundVar
  | otherwise = do
      let args = typeRepArgs tr
      let param = fromJust (head args)
      let rest = last args
      newTypeRequ <- genTypeRequester env depthLeft param boundVar
      ret <- genLambdaSymbol' rest v (trs ++ [newTypeRequ]) env depthLeft target boundVar
      return ret

genLambdaVar :: LambdaEnviroment -> Int -> TypeRep -> BoundVars -> RVar LambdaExpression
genLambdaVar env@(LambdaEnviroment functions constants _ _ weights) depthLeft target boundVar = do
  let availTypes = filter (doTypesMatch target) boundVar
  choosenType <- randomElement $ availTypes
  let tCount = count boundVar choosenType
  indexV <- uniform 0 (tCount-1)
  ret <- genLambdaVar' choosenType choosenType indexV [] env depthLeft target boundVar
  return ret

genLambdaVar' :: TypeRep -> TypeRep -> Int -> [TypeRequester] -> LambdaEnviroment -> Int -> TypeRep -> BoundVars -> RVar LambdaExpression
genLambdaVar' tr varType varNumber trs env@(LambdaEnviroment functions constants _ _ weights) depthLeft target boundVar
  | tr == target = do
      return $ Var varType varNumber trs boundVar
  | otherwise = do
      let args = typeRepArgs tr
      let param = fromJust (head args)
      let rest = last args
      newTypeRequ <- genTypeRequester env depthLeft param boundVar
      ret <- genLambdaVar' rest varType varNumber (trs ++ [newTypeRequ]) env depthLeft target boundVar
      return ret

instance Environment TypeRequester LambdaEnviroment where
  new env@(LambdaEnviroment _ _ target maxDepth _) = do
    tr <- genTypeRequester env maxDepth target []
    return tr

  mutate env@(LambdaEnviroment _ _ _ maxDepth _) tr = do
    let trCount = countTrsR(tr)
    selectedTR <- uniform 1 trCount
    let (depthAt,(TR trep _ bound)) = depthLeftAndTypeAtR tr selectedTR maxDepth
    res <- genTypeRequester env depthAt trep bound
    return $ replaceAtR selectedTR tr res

  nX _ = 3 --todo!

  crossover1 env@(LambdaEnviroment _ _ _ maxDepth _) tr1 tr2 = do
    return Nothing

instance Evaluator TypeRequester LamdaExecutionEnv where
  fitness env tr = (results env) Map.! tr

  calc env pop = do
    let toAdd = NE.filter (\k -> Map.member k (results env) ) pop
    let insertPair (key, val) m = Map.insert key val m
    toInsert <- Hint.runInterpreter (evalResults env toAdd)
    let res = foldr insertPair (results env) (fromRight undefined toInsert)
    return env {results = res}

--  let trCount = countTrsR tr1
--  selectedIndex1 <- uniform 1 trCount
--  let (depthAt, selectedTr1@(TR trep _ bound)) = depthLeftAndTypeAtR tr selectedTR maxDepth
--  let indexes = findIndicesWhere tr2 ( == trep)
--  if length indexes == 0 then return Nothing else (do
--    (selectedTr2,selectedIndex2) <- randomElement indexes)

evalResults :: LamdaExecutionEnv -> [TypeRequester] -> Hint.InterpreterT IO [(TypeRequester, R)]
evalResults ex trs = mapM (evalResult ex) trs

data IrisClass = Setosa | Virginica | Versicolor deriving (Eq, Generic, Show)

instance FromRecord IrisClass
instance ToRecord IrisClass

evalResult :: LamdaExecutionEnv -> TypeRequester -> Hint.InterpreterT IO (TypeRequester, R)
evalResult ex tr = do
  Hint.loadModules (map show (imports ex))
  result <- Hint.interpret (show (toLambdaExpressionS tr)) (Hint.as ::R -> R -> R -> IrisClass)
  csv <- liftIO $ B.readFile (trainingDataset ex)
  let recs = (toList $ fromRight undefined $ decode NoHeader csv) :: [(R,R,R)]
  let res = map ((uncurry3 result)) recs
  csvRes <- liftIO $ B.readFile (trainingDatasetRes ex)
  let recsRes = (toList $ fromRight undefined $ decode NoHeader csvRes) :: [IrisClass]
  let score = (foldr (\ts s -> if (fst ts) == (snd ts) then s + 1 else s - 1) 0 (zip recsRes res)) :: R
  return (tr, score)




-- helper
--findIndicesWhere:: TypeRequester -> (TypeRep -> Bool) -> Int -> [(TypeRequester, Int)]
--findIndicesWhere tr@(TR t lE _) filte indx = case lE of
--                            Just le -> (tr, indx+1):(findIndicesWhere' (asList le) filte (indx+1))
--                            Nothing -> undefined

--findIndicesWhere':: [TypeRequester] -> (TypeRep -> Bool) -> Int -> [(TypeRequester, Int)]
--findIndicesWhere' (tr:trs) f indx = (findIndicesWhere tr f indx) ++ (findIndicesWhere' trs f (indx + countTrsR tr))


replaceAtR:: Int -> TypeRequester -> TypeRequester -> TypeRequester
replaceAtR 0 _ with = with
replaceAtR i (TR tm (Just le) bV) with = TR tm (Just (replaceAt (i-1) le with)) bV
replaceAtR _ (TR _ Nothing _) _ = undefined

-- LambdaSpucker TypeRequester TypeRequester BoundVars | LambdaSchlucker TypeRequester BoundVars | Symbol ConVal [TypeRequester] BoundVars | Var TypeRep Int [TypeRequester] BoundVars | Constan ConVal

replaceAt:: Int -> LambdaExpression -> TypeRequester -> LambdaExpression
replaceAt i le@(LambdaSpucker _ _ bv) with = LambdaSpucker (fromJust (head trs)) (last trs) bv where trs = replaceInSubtreeWithIndex i (asList le) with
replaceAt i (LambdaSchlucker tr bv) with = LambdaSchlucker (replaceAtR i tr with) bv
replaceAt i le@(Symbol cv _ bv) with = Symbol cv trs bv where trs = replaceInSubtreeWithIndex i (asList le) with
replaceAt i le@(Var tr ix _ bv) with = Var tr ix trs bv where trs = replaceInSubtreeWithIndex i (asList le) with
replaceAt _ (Constan _) _ = undefined



replaceInSubtreeWithIndex :: Int -> [TypeRequester] -> TypeRequester  -> [TypeRequester]
replaceInSubtreeWithIndex indexLeft (tr:trs) with = if countTrsR tr >= indexLeft then (replaceAtR indexLeft tr with):trs else tr:(replaceInSubtreeWithIndex (indexLeft - countTrsR tr) trs with)
replaceInSubtreeWithIndex _ [] _ = undefined

depthLeftAndTypeAtR::TypeRequester -> Int -> Int -> (Int, TypeRequester)
depthLeftAndTypeAtR t 0 depthLeft = ((depthLeft - 1), t)
depthLeftAndTypeAtR (TR _ (Just le)  _) indexLeft depthLeft = depthLeftAndTypeAt le (indexLeft - 1) (depthLeft - 1)
depthLeftAndTypeAtR (TR _ Nothing  _) indexLeft depthLeft = undefined


depthLeftAndTypeAt :: LambdaExpression -> Int -> Int -> (Int, TypeRequester)
depthLeftAndTypeAt le indexLeft depthLeft = depthLeftAndTypeInSubtreeWithIndex (asList le) indexLeft depthLeft

depthLeftAndTypeInSubtreeWithIndex :: [TypeRequester] -> Int -> Int -> (Int, TypeRequester)
depthLeftAndTypeInSubtreeWithIndex (tr:trs) indexLeft depthLeft = if countTrsR tr >= indexLeft then depthLeftAndTypeAtR tr indexLeft depthLeft else depthLeftAndTypeInSubtreeWithIndex trs (indexLeft - countTrsR tr) depthLeft
depthLeftAndTypeInSubtreeWithIndex [] indexLeft depthLeft = undefined


countTrsR:: TypeRequester -> Int
countTrsR tr@(TR t lE _) = case lE of
                            Just le -> countTrs le + 1
                            Nothing -> 1

countTrs:: LambdaExpression -> Int
countTrs le = sum (map countTrsR (asList le))

repeatedly :: (a -> Maybe a) -> a -> [a]
repeatedly f x = case f x of
  Nothing -> []
  Just y -> y : repeatedly f y

count :: (Eq a) => [a] -> a -> Int
count [] find = 0
count ys find = length xs
  where
    xs = [xs | xs <- ys, xs == find]

-- Test Stuff

testConstInt :: TypeRequester
testConstInt = TR (Ref.SomeTypeRep (Ref.TypeRep @Int)) (Just (Symbol ("5") [] [])) []

testIntToClassCons :: TypeRequester
testIntToClassCons = TR (Ref.SomeTypeRep (Ref.TypeRep @(Int -> ResClass))) (Just (Symbol ("Class1") [] [])) []

testIntToClassCorrect :: TypeRequester
testIntToClassCorrect =
  TR
    (Ref.SomeTypeRep (Ref.TypeRep @(Int -> ResClass)))
    ( Just
        ( LambdaSchlucker
            ( TR
                (Ref.SomeTypeRep (Ref.TypeRep @(ResClass)))
                ( Just
                    ( Symbol
                        ("iteClass")
                        [ ( TR
                              (Ref.SomeTypeRep (Ref.TypeRep @(Bool)))
                              ( Just
                                  ( Symbol
                                      ("eqInt")
                                      [ ( TR
                                            (Ref.SomeTypeRep (Ref.TypeRep @(Int)))
                                            (Just (Var (Ref.SomeTypeRep (Ref.TypeRep @(Int))) 0 [] []))
                                            [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
                                        ),
                                        ( TR
                                            (Ref.SomeTypeRep (Ref.TypeRep @(Int)))
                                            (Just (Constan ("1")))
                                            [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
                                        )
                                      ]
                                      [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
                                  )
                              )
                              [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
                          ),
                          ( TR
                              (Ref.SomeTypeRep (Ref.TypeRep @(ResClass)))
                              (Just (Constan ("Class1")))
                              [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
                          ),
                          ( TR
                              (Ref.SomeTypeRep (Ref.TypeRep @(ResClass)))
                              ( Just
                                  ( Symbol
                                      ("iteClass")
                                      [ ( TR
                                            (Ref.SomeTypeRep (Ref.TypeRep @(Bool)))
                                            ( Just
                                                ( Symbol
                                                    ("eqInt")
                                                    [ ( TR
                                                          (Ref.SomeTypeRep (Ref.TypeRep @(Int)))
                                                          (Just (Var (Ref.SomeTypeRep (Ref.TypeRep @(Int))) 0 [] []))
                                                          [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
                                                      ),
                                                      ( TR
                                                          (Ref.SomeTypeRep (Ref.TypeRep @(Int)))
                                                          (Just (Constan ("2")))
                                                          [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
                                                      )
                                                    ]
                                                    [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
                                                )
                                            )
                                            [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
                                        ),
                                        ( TR
                                            (Ref.SomeTypeRep (Ref.TypeRep @(ResClass)))
                                            (Just (Constan ("Class2")))
                                            [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
                                        ),
                                        ( TR
                                            (Ref.SomeTypeRep (Ref.TypeRep @(ResClass)))
                                            ( Just
                                                ( Symbol
                                                    ("iteClass")
                                                    [ ( TR
                                                          (Ref.SomeTypeRep (Ref.TypeRep @(Bool)))
                                                          ( Just
                                                              ( Symbol
                                                                  ("eqInt")
                                                                  [ ( TR
                                                                        (Ref.SomeTypeRep (Ref.TypeRep @(Int)))
                                                                        (Just (Var (Ref.SomeTypeRep (Ref.TypeRep @(Int))) 0 [] []))
                                                                        [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
                                                                    ),
                                                                    ( TR
                                                                        (Ref.SomeTypeRep (Ref.TypeRep @(Int)))
                                                                        (Just (Constan ("3")))
                                                                        [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
                                                                    )
                                                                  ]
                                                                  [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
                                                              )
                                                          )
                                                          [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
                                                      ),
                                                      ( TR
                                                          (Ref.SomeTypeRep (Ref.TypeRep @(ResClass)))
                                                          (Just (Constan ("Class3")))
                                                          [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
                                                      ),
                                                      ( TR
                                                          (Ref.SomeTypeRep (Ref.TypeRep @(ResClass)))
                                                          (Just (Constan ("Class3")))
                                                          [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
                                                      )
                                                    ]
                                                    [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
                                                )
                                            )
                                            [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
                                        )
                                      ]
                                      [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
                                  )
                              )
                              [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
                          )
                        ]
                        [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
                    )
                )
                [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
            )
            [(Ref.SomeTypeRep (Ref.TypeRep @(Int)))]
        )
    )
    []

data ResClass = Class1 | Class2 | Class3 deriving (Enum, Show)

eqInt :: Int -> Int -> Bool
eqInt a b = a == b

iteClass :: Bool -> ResClass -> ResClass -> ResClass
iteClass True c _ = c
iteClass False _ c = c

toLambdaExpressionShort :: TypeRequester -> Text
toLambdaExpressionShort (TR _ (Just lambdaExpression) _) = "(" <> eToLambdaExpressionShort lambdaExpression <> ")"
toLambdaExpressionShort (TR _ (Nothing) _) = "Invalid Lambda Epr"

-- data LambdaExpression = LambdaSpucker TypeRequester TypeRequester BoundVars | LambdaSchlucker TypeRequester BoundVars | Symbol ConVal [TypeRequester] BoundVars | Var TypeRep Int | Constan ConVal

eToLambdaExpressionShort :: LambdaExpression -> Text
eToLambdaExpressionShort (LambdaSpucker typeRequester1 typeRequester2 boundVars) = "(\\l" <> showSanifid (last boundVars) <> show (count boundVars (last boundVars) - 1) <> " -> " <> toLambdaExpressionShort typeRequester2 <> ") " <> toLambdaExpressionShort typeRequester1
eToLambdaExpressionShort (LambdaSchlucker typeRequester boundVars) = "()\\l" <> showSanifid (last boundVars) <> show (count boundVars (last boundVars) - 1) <> " -> " <> toLambdaExpressionShort typeRequester <> ")"
eToLambdaExpressionShort (Symbol (valS) typeRequesters _) = valS <> " " <> (unwords (map toLambdaExpressionShort typeRequesters))
eToLambdaExpressionShort (Var typeRep int typeRequesters _) = "l" <> showSanifid typeRep <> show int <> " " <> (unwords (map toLambdaExpressionShort typeRequesters))
eToLambdaExpressionShort (Constan (valS)) = valS

res :: Int -> ResClass
res = ((\lInt0 -> ((iteClass ((eqInt ((lInt0) :: (Int)) ((1) :: (Int))) :: (Bool)) ((Class1) :: (ResClass)) ((iteClass ((eqInt ((lInt0) :: (Int)) ((2) :: (Int))) :: (Bool)) ((Class2) :: (ResClass)) ((iteClass ((eqInt ((lInt0) :: (Int)) ((3) :: (Int))) :: (Bool)) ((Class3) :: (ResClass)) ((Class3) :: (ResClass))) :: (ResClass))) :: (ResClass))) :: (ResClass))) :: (Int -> ResClass))
