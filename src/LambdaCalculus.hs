{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LambdaCalculus where

import Data.List (foldr1, last, nub, intersect, (!!), (\\))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Random
import qualified Data.Text as T
import Data.Tuple.Extra
import Data.Typeable
import GA
import Pretty
import Protolude
import Protolude.Error
import Debug.Trace as DB
import Test.QuickCheck hiding (sample, shuffle)
import Test.QuickCheck.Monadic (assert, monadicIO)
import qualified Type.Reflection as Ref

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

showSanifid :: (Show a) => a -> Text
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
            lambdaSchlucker = 2,
            symbol = 2,
            variable = 10,
            constant = 2
          }
    }

type BoundVars = [TypeRep]


-- we need a dynamic typ with a concept of equality here, should we want to interpret the result, instead of compiling it...
type ConVal = Text

-- LambdaSpucker - adds TypeRequester#1 as bound var and returns the result of TypeRequester#2

data LambdaExpression = LambdaSpucker TypeRequester TypeRequester BoundVars | LambdaSchlucker TypeRequester BoundVars | Symbol ConVal [TypeRequester] BoundVars | Var TypeRep Int [TypeRequester] BoundVars | Constan ConVal deriving (Eq, Ord, Show)

asList :: LambdaExpression -> [TypeRequester]
asList (LambdaSpucker tr1 tr2 _) = [tr1, tr2]
asList (LambdaSchlucker tr _) = [tr]
asList (Symbol _ trs _) = trs
asList (Var _ _ trs _) = trs
asList (Constan _) = []

data TypeRequester = TR TypeRep (Maybe LambdaExpression) BoundVars deriving (Eq, Ord, Show)

toLambdaExpressionS :: TypeRequester -> Text
toLambdaExpressionS (TR typeRep (Just lambdaExpression) boundVars) = "((" <> eToLambdaExpressionS lambdaExpression <> ") :: (" <> show typeRep <> "))"
toLambdaExpressionS (TR _ (Nothing) _) = "Invalid Lambda Epr"

-- data LambdaExpression = LambdaSpucker TypeRequester TypeRequester BoundVars | LambdaSchlucker TypeRequester BoundVars | Symbol ConVal [TypeRequester] BoundVars | Var TypeRep Int [TypeRequester] BoundVars | Constan ConVal deriving (Eq, Ord, Show)


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
  lamdaTypeLength <- uniform 1 4
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
  indexV <- uniform 0 (tCount - 1)
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
    let trCount = countTrsR (tr)
    selectedTR <- uniform 1 trCount
    let (depthAt, (TR trep _ bound)) = depthLeftAndTypeAtR tr selectedTR maxDepth
    res <- genTypeRequester env depthAt trep bound
    return $ replaceAtR selectedTR tr res

  nX _ = 3 -- todo!

  crossover1 env@(LambdaEnviroment _ _ _ maxDepth _) tr1 tr2 = do
    let trCount = countTrsR tr1
    selectedIndex1 <- uniform 1 trCount
    let (depthAt, selectedTr1@(TR _ _ bound1)) = depthLeftAndTypeAtR tr1 selectedIndex1 maxDepth
    let indexes = findIndicesWhere tr2 (isCompatibleTr selectedTr1) 0
    if length indexes == 0 then return Nothing else (do
      (selectedTr2@(TR _ _ bound2),selectedIndex2) <- randomElement indexes
      selectedTr2 <- adaptBoundVars selectedTr2 bound1
      selectedTr1 <- adaptBoundVars selectedTr1 bound2
      let child1 = replaceAtR selectedIndex1 tr1 selectedTr2
      let child2 = replaceAtR selectedIndex2 tr2 selectedTr1
      return $ Just (child1, child2)
      )


-- helper
adaptBoundVars:: TypeRequester -> BoundVars -> RVar TypeRequester
adaptBoundVars tr@(TR _ _ bvOld) bvNew = do
  newIndexMap <- generateConversionIndexMap bvOld bvNew
  return $ convertTr tr bvOld bvNew newIndexMap

convertTr:: TypeRequester -> BoundVars -> BoundVars -> Map TypeRep (Int -> Int) -> TypeRequester
convertTr tr@(TR tRp (Just le) bvCurr) bvOld bvNew mapper = TR tRp (Just (convertLe le bvOld bvNew mapper)) (bvNew ++ (bvCurr \\ bvOld))
convertTr _ _ _ _ = error "le Not Just (convertTr)"


-- data LambdaExpression = LambdaSpucker TypeRequester TypeRequester BoundVars | LambdaSchlucker TypeRequester BoundVars | Symbol ConVal [TypeRequester] BoundVars | Var TypeRep Int [TypeRequester] BoundVars | Constan ConVal deriving (Eq, Ord, Show)
convertLe:: LambdaExpression -> BoundVars -> BoundVars -> Map TypeRep (Int -> Int) -> LambdaExpression
convertLe (LambdaSpucker tr1 tr2 bvCurr) bvOld bvNew mapper = LambdaSpucker (convertTrf tr1) (convertTrf tr2) (bvNew ++ (bvCurr \\ bvOld))
  where convertTrf tr = convertTr tr bvOld bvNew mapper
convertLe (LambdaSchlucker tr bvCurr) bvOld bvNew mapper = LambdaSchlucker (convertTrf tr) (bvNew ++ (bvCurr \\ bvOld))
  where convertTrf tr = convertTr tr bvOld bvNew mapper
convertLe (Symbol cv trs bvCurr) bvOld bvNew mapper = Symbol cv (map convertTrf trs) (bvNew ++ (bvCurr \\ bvOld))
  where convertTrf tr = convertTr tr bvOld bvNew mapper
convertLe (Var varType varNumber trs bvCurr) bvOld bvNew mapper = Var varType ((fromMaybe identity (Map.lookup varType mapper)) varNumber) (map convertTrf trs) (bvNew ++ (bvCurr \\ bvOld))
  where convertTrf tr = convertTr tr bvOld bvNew mapper
convertLe le@(Constan _) _ _ _ = le


generateConversionIndexMap:: BoundVars -> BoundVars -> RVar (Map TypeRep (Int -> Int))
generateConversionIndexMap bvOld bvNew = do
  funcs <- mapM (\bT -> genMapper (count bvOld bT - 1) (count bvNew bT - 1)) (nub bvOld)
  return $ Map.fromList $ zip (nub bvOld) funcs

genMapper:: Int -> Int -> RVar (Int -> Int)
genMapper i j | i == j = return identity
              | i < j = return $ \int -> if int <= i then int else int + (j-i)
              | i > j = do
                  permutationForUnbound <- genPermutation i j
                  return $ genMapperRandomAssment i j permutationForUnbound
              | otherwise = error "impossible case in genMapper"

genMapperRandomAssment:: Int -> Int -> [Int] -> Int -> Int
genMapperRandomAssment i j permutationForUnbound int | int <= j = int
                                                     | int > i = int - (i-j)
                                                     | otherwise = permutationForUnbound !! (int - j - 1)

genPermutation:: Int -> Int -> RVar [Int]
genPermutation i j = replicateM (i - j) (uniform 0 j)

isCompatibleTr:: TypeRequester -> TypeRequester -> Bool
isCompatibleTr tr1@(TR trep1 _ bound1) tr2@(TR trep2 _ bound2) | trep1 == trep2 = allUsedBound (usedVars bound1 tr1) bound2 && allUsedBound (usedVars bound2 tr2) bound1
                                                       | otherwise = False
allUsedBound :: BoundVars -> BoundVars -> Bool
allUsedBound used available = all (\x -> any (== x) available) used


usedVars :: BoundVars -> TypeRequester -> BoundVars
usedVars boundOld tr@(TR trep1 (Just (Var trp ind trs _)) _) = if any (== trp) boundOld && count boundOld trp > ind then trp : concatMap (usedVars boundOld) trs else concatMap (usedVars boundOld) trs
usedVars boundOld tr@(TR trep1 (Just le) _) =  concatMap (usedVars boundOld) (asList le)
usedVars _ _ = error "Nothing in usedVars"


boundsConvertable:: BoundVars -> BoundVars -> Bool
boundsConvertable bv1 bv2 =  length (nub bv2) == length (nub bv1) && length (intersect (nub bv1) bv2) == length (nub bv1)


findIndicesWhere:: TypeRequester -> (TypeRequester -> Bool) -> Int -> [(TypeRequester, Int)]
findIndicesWhere tr@(TR t lE _) filte indx = case lE of
                           Just le -> if filte tr then (tr, indx+1):(findIndicesWhere' (asList le) filte (indx+1)) else (findIndicesWhere' (asList le) filte (indx+1))
                           Nothing -> error "Nothing in findIndicesWhere"

findIndicesWhere':: [TypeRequester] -> (TypeRequester -> Bool) -> Int -> [(TypeRequester, Int)]
findIndicesWhere' [] _ _ = []
findIndicesWhere' [tr] f indx = (findIndicesWhere tr f indx)
findIndicesWhere' (tr:trs) f indx = (findIndicesWhere tr f indx) ++ (findIndicesWhere' trs f (indx + countTrsR tr))

replaceAtR :: Int -> TypeRequester -> TypeRequester -> TypeRequester
replaceAtR 1 _ with = with
replaceAtR i (TR tm (Just le) bV) with = TR tm (Just (replaceAt (i - 1) le with)) bV
replaceAtR _ (TR _ Nothing _) _ = error "Nothing in replaceAtR"

replaceAt :: Int -> LambdaExpression -> TypeRequester -> LambdaExpression
replaceAt i le@(LambdaSpucker _ _ bv) with = LambdaSpucker (fromJust (head trs)) (last trs) bv where trs = replaceInSubtreeWithIndex i (asList le) with
replaceAt i (LambdaSchlucker tr bv) with = LambdaSchlucker (replaceAtR i tr with) bv
replaceAt i le@(Symbol cv _ bv) with = Symbol cv trs bv where trs = replaceInSubtreeWithIndex i (asList le) with
replaceAt i le@(Var tr ix _ bv) with = Var tr ix trs bv where trs = replaceInSubtreeWithIndex i (asList le) with
replaceAt _ (Constan _) _ = error "Nothing in replaceAt"

replaceInSubtreeWithIndex :: Int -> [TypeRequester] -> TypeRequester -> [TypeRequester]
replaceInSubtreeWithIndex indexLeft (tr : trs) with = if countTrsR tr >= indexLeft then (replaceAtR indexLeft tr with) : trs else tr : (replaceInSubtreeWithIndex (indexLeft - countTrsR tr) trs with)
replaceInSubtreeWithIndex _ [] _ = error "Index not found in replaceInSubtreeWithIndex"

depthLeftAndTypeAtR :: TypeRequester -> Int -> Int -> (Int, TypeRequester)
depthLeftAndTypeAtR t 1 depthLeft = ((depthLeft - 1), t)
depthLeftAndTypeAtR (TR _ (Just le) _) indexLeft depthLeft = depthLeftAndTypeAt le (indexLeft - 1) (depthLeft - 1)
depthLeftAndTypeAtR (TR _ Nothing _) indexLeft depthLeft = error "Nothing in depthLeftAndTypeAtR"

depthLeftAndTypeAt :: LambdaExpression -> Int -> Int -> (Int, TypeRequester)
depthLeftAndTypeAt le indexLeft depthLeft = depthLeftAndTypeInSubtreeWithIndex (asList le) indexLeft depthLeft

depthLeftAndTypeInSubtreeWithIndex :: [TypeRequester] -> Int -> Int -> (Int, TypeRequester)
depthLeftAndTypeInSubtreeWithIndex (tr : trs) indexLeft depthLeft = if countTrsR tr >= indexLeft then depthLeftAndTypeAtR tr indexLeft depthLeft else depthLeftAndTypeInSubtreeWithIndex trs (indexLeft - countTrsR tr) depthLeft
depthLeftAndTypeInSubtreeWithIndex [] indexLeft depthLeft = error "Index not found in depthLeftAndTypeInSubtreeWithIndex"

countTrsR :: TypeRequester -> Int
countTrsR tr@(TR t lE _) = case lE of
  Just le -> countTrs le + 1
  Nothing -> 1

countTrs :: LambdaExpression -> Int
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
eToLambdaExpressionShort (LambdaSchlucker typeRequester boundVars) = "(\\l" <> showSanifid (last boundVars) <> show (count boundVars (last boundVars) - 1) <> " -> " <> toLambdaExpressionShort typeRequester <> ")"
eToLambdaExpressionShort (Symbol (valS) typeRequesters _) = valS <> " " <> (unwords (map toLambdaExpressionShort typeRequesters))
eToLambdaExpressionShort (Var typeRep int typeRequesters _) = "l" <> showSanifid typeRep <> show int <> " " <> (unwords (map toLambdaExpressionShort typeRequesters))
eToLambdaExpressionShort (Constan (valS)) = valS

res :: Int -> ResClass
res = ((\lInt0 -> ((iteClass ((eqInt ((lInt0) :: (Int)) ((1) :: (Int))) :: (Bool)) ((Class1) :: (ResClass)) ((iteClass ((eqInt ((lInt0) :: (Int)) ((2) :: (Int))) :: (Bool)) ((Class2) :: (ResClass)) ((iteClass ((eqInt ((lInt0) :: (Int)) ((3) :: (Int))) :: (Bool)) ((Class3) :: (ResClass)) ((Class3) :: (ResClass))) :: (ResClass))) :: (ResClass))) :: (ResClass))) :: (Int -> ResClass))


meanOfAccuricyPerClass :: (Enum r, Bounded r, Eq r) => [(r, r)] -> R
meanOfAccuricyPerClass results = mean $ map (accuracyInClass results) [minBound .. maxBound]

geomeanOfAccuricyPerClass :: (Enum r, Bounded r, Eq r) => [(r, r)] -> R
geomeanOfAccuricyPerClass results = geomean $ map (accuracyInClass results) [minBound .. maxBound]

geomeanOfDistributionAccuracy :: (Enum r, Bounded r, Eq r) => [(r, r)] -> R
geomeanOfDistributionAccuracy results = geomean $ map (distributionAccuracyForClass results) [minBound .. maxBound]

distributionAccuracyForClass :: (Eq r) => [(r, r)] -> r -> R
distributionAccuracyForClass results clas = (1 - (min 1 (fromIntegral (abs ((length (inResClass results clas)) - (length (inClass results clas)))) / fromIntegral (length (inClass results clas))))) * 100

mean :: (Show f, Floating f) => [f] -> f
mean values = (sum values) * (1 / (fromIntegral (length values)))

geomean :: (Show f, Floating f) => [f] -> f
geomean values = (product values) ** (1 / (fromIntegral (length values)))

accuracyInClass :: (Eq r) => [(r, r)] -> r -> R
accuracyInClass results clas = ((accuracy'(inResClass results clas)) * 100) / fromIntegral (length (inClass results clas))

inClass :: (Eq r) => [(r, r)] -> r -> [(r, r)]
inClass results clas = (filter ((clas ==) . fst) results)

inResClass :: (Eq r) => [(r, r)] -> r -> [(r, r)]
inResClass results clas = (filter ((clas ==) . snd) results)

accuracy' :: (Eq r) => [(r, r)] -> R
accuracy' results = fromIntegral $ length (filter (\(target, res) -> (res == target)) results)
