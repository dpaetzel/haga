{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}

module LambdaCalculusV2 where

import Data.Dynamic
import Data.Kind
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Random
import Data.Typeable
import Debug.Trace as DB
import qualified Data.Text as T
import GA
import Protolude
import Protolude.Error
import Protolude.Partial
import qualified Type.Reflection as Ref
import Utils

data BoundSymbol where
  BoundSymbol :: (Typeable a) => Ref.TypeRep a -> a -> Maybe Text -> BoundSymbol

type Bindings = Map.Map (Ref.SomeTypeRep) Int

data SomeSimplyTypedLambdaExpression where
  SomeSimplyTypedLambdaExpression :: (Typeable a) => SimplyTypedLambdaExpression a -> SomeSimplyTypedLambdaExpression

-- We specify a and use GADTs to allow Haskell to guarantee full type safety over these expressions!
-- This gurantees us that a SimplyTypedLambdaExpression a describes a lambda expression of type a!
data SimplyTypedLambdaExpression t where
  Application :: (Typeable a, Typeable b) => SimplyTypedLambdaExpression (a -> b) -> SimplyTypedLambdaExpression a -> SimplyTypedLambdaExpression b -- e = e1 e2
  Abstraction :: (Typeable (a -> b), Typeable b) => Ref.TypeRep a -> SimplyTypedLambdaExpression (b) -> SimplyTypedLambdaExpression (a -> b) -- e = λx:a. e
  VariableReference :: (Typeable a) => Ref.TypeRep a -> Int -> SimplyTypedLambdaExpression a -- e = x this Includes predefined function use!
  Constant :: (Typeable a, Ord a, Hashable a, Show a) => a -> SimplyTypedLambdaExpression a -- e = c

instance Eq (SimplyTypedLambdaExpression t) where
  e1 == e2 = compare e1 e2 == EQ

instance Ord (SimplyTypedLambdaExpression t) where
  compare (Application (stleAtoB1 :: SimplyTypedLambdaExpression (a1 -> t)) (stleA1 :: SimplyTypedLambdaExpression a1)) (Application (stleAtoB2 :: SimplyTypedLambdaExpression (a2 -> t)) (stleA2 :: SimplyTypedLambdaExpression a2)) = case eqT @a1 @a2 of
    Just Refl -> (compare stleAtoB1 stleAtoB2) `thenCmp` (compare stleA1 stleA2)
    _ -> compare (Ref.SomeTypeRep (Ref.TypeRep @a1)) (Ref.SomeTypeRep (Ref.TypeRep @a2))
  compare (Abstraction rep1 stle1) (Abstraction rep2 stle2) = (compare rep1 rep2) `thenCmp` (compare stle1 stle2)
  compare (VariableReference repA inx1) (VariableReference repB inx2) = (compare repA repB) `thenCmp` (compare inx1 inx2)
  compare (Constant res1) (Constant res2) = compare res1 res2
  compare (Application _ _) _ = LT
  compare _ (Application _ _) = GT
  compare (Abstraction _ _) _ = LT
  compare _ (Abstraction _ _) = GT
  compare (VariableReference _ _) _ = LT
  compare _ (VariableReference _ _) = GT

instance Hashable (SimplyTypedLambdaExpression t) where
  hashWithSalt salt (Application stleAtoB stleA) = salt `hashWithSalt` (1 :: Int) `hashWithSalt` stleAtoB `hashWithSalt` stleA
  hashWithSalt salt (Abstraction rep stle) = salt `hashWithSalt` (2 :: Int) `hashWithSalt` rep `hashWithSalt` stle
  hashWithSalt salt (VariableReference rep inx) = salt `hashWithSalt` (3 :: Int) `hashWithSalt` rep `hashWithSalt` inx
  hashWithSalt salt (Constant res) = salt `hashWithSalt` (4 :: Int) `hashWithSalt` res

thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ o2 = o2
thenCmp o1 _ = o1

data ConstVal where
  ConstVal :: (Typeable a, Ord a, Hashable a, Show a) => Ref.TypeRep a -> RVar a -> ConstVal

data ExpressionWeights = ExpressionWeights
  { application :: Int,
    abstraction :: Int,
    variableReference :: Int,
    constant :: Int,
    -- chance in percent an Application will (try to) work towards something from the boundVars becoming usable. I recommend values over 90.
    functionBias :: Int
  }

data LambdaEnviroment a = LambdaEnviroment
  { functions :: [BoundSymbol],
    constants :: [ConstVal],
    maxDepth :: Int,
    weights :: ExpressionWeights,
    --  likelyhood of an sub-expression to be mutated
    mutationStrength :: Float,
    --  likelyhood of an crossover attempt at a sub-expression
    crossoverStrength :: Float
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

data Dataset t where
  Input :: (Typeable a, Typeable b) => [a] -> Dataset b -> Dataset (a -> b)
  Result :: (Typeable a, Eq a, Enum a, Bounded a) => [a] -> Dataset a

data ExecutionEnviroment e = ExecutionEnviroment {
    fun :: [BoundSymbol],
    training :: Bool,
    trainingData :: Dataset e,
    testData :: Dataset e
  }

data ResultList where
  Res :: (Typeable a, Eq a, Enum a, Bounded a) => [(a,a)] -> ResultList

instance Typeable a => Evaluator (SimplyTypedLambdaExpression a) (ExecutionEnviroment a) FittnesRes where
  fitness' ee@(ExecutionEnviroment {fun}) e = evalResult ee e (eval fun e)

evalResult :: ExecutionEnviroment a -> SimplyTypedLambdaExpression a -> a -> FittnesRes
evalResult (ExecutionEnviroment {training, trainingData, testData}) tr result = FittnesRes
        { total = (\(Res r) -> meanOfDistributionAccuracy r) res,
          fitnessTotal = fitness',
          fitnessMean = (\(Res r) -> meanOfAccuricyPerClass r) res ,
          fitnessGeoMean = (\(Res r) -> meanOfDistributionAccuracy r) res,
          accuracy = acc,
          biasSize = biasSmall,
          totalSize = expSize tr
        }
    where
    dataS = (if training then trainingData else testData)
    res = apply result dataS
    acc = (\(Res r) -> (foldr (\(ts) s -> if ((fst ts) == (snd ts)) then s + 1 else s) 0 r) / fromIntegral (length r)) res
    biasSmall = exp ((-(fromIntegral (expSize tr))) / 1000) -- 0 (schlecht) bis 1 (gut)
    fitness' = (\(Res r) -> meanOfAccuricyPerClass r) res
    score = fitness' + (biasSmall - 1)

apply :: a -> Dataset a -> ResultList
apply fun (Input b c) = applyL (map fun b) c
apply val (Result b) = Res (zip b (repeat val))

applyL :: [a] -> Dataset a -> ResultList
applyL fun (Input b c) = applyL (zipWith (\a b -> a b) fun b) c
applyL val (Result b) = Res (zip b val)



hasSymbolOfType :: forall (a :: Type). [BoundSymbol] -> Ref.TypeRep a -> Bool
hasSymbolOfType bound tr = length ((getSymbolsOfType bound tr) :: [a]) /= 0

getSymbolsOfType :: forall a. [BoundSymbol] -> Ref.TypeRep a -> [a]
getSymbolsOfType bound tr = mapMaybe (getIfType tr) bound

getBoundSymbolsOfType :: forall a. [BoundSymbol] -> Ref.TypeRep a -> [BoundSymbol]
getBoundSymbolsOfType bound tr = mapMaybe (getSymbolIfType tr) bound

getSymbolIfType :: forall a. Ref.TypeRep a -> BoundSymbol -> Maybe BoundSymbol
getSymbolIfType rep b@(BoundSymbol t _ _)
  | Just Ref.HRefl <- t `Ref.eqTypeRep` rep = Just b
  | otherwise = Nothing

getIfType :: forall a. Ref.TypeRep a -> BoundSymbol -> Maybe a
getIfType rep (BoundSymbol t val _)
  | Just Ref.HRefl <- t `Ref.eqTypeRep` rep = Just val
  | otherwise = Nothing

startingBindings :: [BoundSymbol] -> Bindings
startingBindings functions = (foldr (\(BoundSymbol tr _ _) map -> Map.insertWith (+) (Ref.SomeTypeRep tr) 1 map) Map.empty functions)

showSanifid :: (Show a) => a -> Text
showSanifid var = T.replace " -> " "To" (show var)

toDotE :: LambdaEnviroment a -> Text
toDotE (LambdaEnviroment {functions}) = foldr (<>) "" (map (\(BoundSymbol tr _ t, inx) -> "\"" <> (showSanifid tr) <> show inx <> "\" [style = invis label = " <> fromJust t <>"\"]\n") (concatMap (\(Ref.SomeTypeRep k,v) -> zip (getBoundSymbolsOfType functions k)[0 .. (v-1)]) (Map.toList (startingBindings functions))))

toDotI :: SimplyTypedLambdaExpression e -> Int -> Text
toDotI (Application e1 e2) inx = "\"app" <> show inx <> "\" -- "  <> toDotI e1 (inx + 1) <> "\n" <> "\"app" <> show inx <> "\" -- "  <> toDotI e2 (inx + 1 + expSize e1)
toDotI (Abstraction _ e) inx = "\"abs" <> show inx <> "\" -- "  <> toDotI e (inx + 1)
toDotI (VariableReference tr i) _ = "\"" <> (showSanifid tr) <> show i <> "\""
toDotI (Constant c) _ = "\"" <> show c <> "\""

instance Eq SomeSimplyTypedLambdaExpression where
  e1 == e2 = compare e1 e2 == EQ

instance Ord SomeSimplyTypedLambdaExpression where
  compare (SomeSimplyTypedLambdaExpression (e1 :: SimplyTypedLambdaExpression a)) (SomeSimplyTypedLambdaExpression (e2 :: SimplyTypedLambdaExpression b))
    | Just Refl <- eqT @a @b = compare e1 e2
    | otherwise = compare (Ref.SomeTypeRep (Ref.TypeRep @a)) (Ref.SomeTypeRep (Ref.TypeRep @b))

instance Typeable a => Individual (SimplyTypedLambdaExpression a)

instance Typeable a => Environment (SimplyTypedLambdaExpression a) (LambdaEnviroment a) where
  output env  i = toDotE env <> toDotI i 0

  nX _ = 3

  new env = DB.trace "new !" ((generateFromEnv env) :: RVar (SimplyTypedLambdaExpression a))

  mutate env le = (mutateUnwrapped env le)

  crossover1 env le  le2 = crossoverUnwrapper env le le2

crossoverUnwrapper :: (Typeable a) => LambdaEnviroment a -> SimplyTypedLambdaExpression a -> SimplyTypedLambdaExpression a -> RVar (Maybe (SimplyTypedLambdaExpression a, SimplyTypedLambdaExpression a))
crossoverUnwrapper env@(LambdaEnviroment {maxDepth, functions}) le1 le2 =
      ( do
          (tree1, tree2) <- crossedover le1 le2 env maxDepth (startingBindings functions)
          return $ if (tree2 == le2) then Nothing else Just (tree1, tree2)
      )

crossedover :: forall a e. (Typeable a,Typeable e) => SimplyTypedLambdaExpression a -> SimplyTypedLambdaExpression e -> LambdaEnviroment e -> Int -> Bindings -> RVar (SimplyTypedLambdaExpression a, SimplyTypedLambdaExpression e)
crossedover le1 le2 env@(LambdaEnviroment {crossoverStrength, maxDepth, functions}) sizeLeft bound = do
  roll <- uniform 0 1
  let crossoverChild =
        ( case le1 of
            (Application e1 e2) ->
              ( do
                  (elm1, partner1) <- crossedover e1 le2 env ((sizeLeft - 1) - expSize e2) bound
                  (elm2, partner2) <- crossedover e2 le2 env ((sizeLeft - 1) - expSize e1) bound
                  leftMutated <- uniform False True
                  let mutateLeft = if partner1 == le2 then False else (if partner2 == le2 then False else leftMutated)
                  return $ if mutateLeft then (Application elm1 e2, partner1) else (Application e1 elm2, partner2)
              )
            (Abstraction tr e) ->
              ( do
                  (elm2, partner2) <- crossedover e le2 env (sizeLeft - 1) (Map.insertWith (+) (Ref.SomeTypeRep tr) 1 bound)
                  return $ (Abstraction tr elm2, partner2)
              )
            _ -> return (le1, le2)
        )
  if (roll < crossoverStrength)
    then
      ( do
          maybeSwapped <- trySwapSubtree le1 sizeLeft bound le2 maxDepth (startingBindings functions)
          case maybeSwapped of
            Just (ler1, ler2) -> return (ler1, ler2)
            _ -> crossoverChild
      )
    else crossoverChild

trySwapSubtree :: forall a e.(Typeable a,Typeable e) =>  SimplyTypedLambdaExpression a -> Int -> Bindings -> SimplyTypedLambdaExpression e -> Int -> Bindings -> RVar (Maybe (SimplyTypedLambdaExpression a, SimplyTypedLambdaExpression e))
trySwapSubtree le1 sizeLeft bound le2 sizeLeft2 bound2 = do
    let possible = possibleSwapSubtrees le1 sizeLeft bound le2 sizeLeft2 bound2
    case possible of
         [] -> return Nothing
         ne -> Just <$> randomElement ne


possibleSwapSubtrees :: forall a e.(Typeable a,Typeable e) => SimplyTypedLambdaExpression a -> Int -> Bindings -> SimplyTypedLambdaExpression e -> Int -> Bindings -> [(SimplyTypedLambdaExpression a, SimplyTypedLambdaExpression e)]
possibleSwapSubtrees le1 sizeLeft bound le2 sizeLeft2 bound2
  | Just Refl <- eqT @a @e =  if compatibleSubtree sizeLeft2 bound2 le1 && compatibleSubtree sizeLeft bound le2 then (adaptSubtree bound2 le1, adaptSubtree bound le2) : continue else continue
  | otherwise = continue
  where
    continue = (case le2 of
          Application e1 e2 -> (map (\(li1,li2) -> (li1, (Application e1 li2))) (possibleSwapSubtrees le1 sizeLeft bound e2 (sizeLeft2 - 1 - expSize e1) bound2) ) ++ (map (\(li1,li2) -> (li1, (Application li2 e2))) (possibleSwapSubtrees le1 sizeLeft bound e1 (sizeLeft2 - 1 - expSize e2) bound2) )
          Abstraction t e -> (map (\(li1,li2) -> (li1, (Abstraction t li2))) (possibleSwapSubtrees le1 sizeLeft bound e (sizeLeft2 - 1) (addToBindings t bound2)))
          _ -> [])

addToBindings ::Ref.TypeRep a -> Bindings -> Bindings
addToBindings t bound =  (Map.insertWith (+) (Ref.SomeTypeRep t) 1 bound)

adaptSubtree :: Bindings -> SimplyTypedLambdaExpression e -> SimplyTypedLambdaExpression e
adaptSubtree bound (Application e1 e2) = (Application (adaptSubtree bound e1) (adaptSubtree bound e2))
adaptSubtree bound (Abstraction t e) = (Abstraction t (adaptSubtree (addToBindings t bound) e))
adaptSubtree bound (VariableReference tr idx) =  (VariableReference tr (mod idx ( bound Map.! (Ref.SomeTypeRep tr))))
adaptSubtree _ e = e

compatibleSubtree :: Int -> Bindings -> SimplyTypedLambdaExpression e -> Bool
compatibleSubtree sizeLeft bound subtree =  bound `bindingContains` (bindingReq subtree) && sizeLeft > (expSize subtree)

expSize :: SimplyTypedLambdaExpression e -> Int
expSize (Application e1 e2) = expSize e1 + expSize e2 + 1
expSize (Abstraction _ e) = expSize e + 1
expSize _ = 1

bindingReq :: SimplyTypedLambdaExpression e -> Bindings
bindingReq (Application e1 e2) = Map.unionWith (max) (bindingReq e1) (bindingReq e2)
bindingReq (Abstraction tr e) = rmFromBindings tr (bindingReq e)
bindingReq (VariableReference tr idx) = Map.singleton (Ref.SomeTypeRep tr) 1
bindingReq (Constant _) = Map.empty

rmFromBindings ::Ref.TypeRep a -> Bindings -> Bindings
rmFromBindings t bound =  (Map.insertWith (\i1 i2 -> max 0 (i1 + i2)) (Ref.SomeTypeRep t) (- 1) bound)

bindingContains :: Bindings -> Bindings -> Bool
bindingContains superset subset = all (\(key,val) -> (fromMaybe 0 (Map.lookup key superset)) >= val ) (Map.toList subset)

mutateUnwrapped :: (Typeable r) => LambdaEnviroment r -> SimplyTypedLambdaExpression r -> RVar (SimplyTypedLambdaExpression r)
mutateUnwrapped env@(LambdaEnviroment {maxDepth, functions}) stle = mutated stle env maxDepth (startingBindings functions)

mutated :: forall r a. (Typeable r) => SimplyTypedLambdaExpression r -> LambdaEnviroment a -> Int -> Bindings -> RVar (SimplyTypedLambdaExpression r)
mutated (Application e1 e2) env@(LambdaEnviroment {constants, mutationStrength}) sizeLeft bound = do
  roll <- uniform 0 1
  if (roll < mutationStrength)
    then generate env (Ref.TypeRep @r) constants sizeLeft bound
    else do
      sizeDistribution <- uniform 0 (sizeLeft - 1)
      elm1 <- mutated e1 env sizeDistribution bound
      elm2 <- mutated e2 env ((sizeLeft - 1) - sizeDistribution) bound
      return $ Application elm1 elm2
mutated (Abstraction tr e) env@(LambdaEnviroment {constants, mutationStrength}) sizeLeft bound = do
  roll <- uniform 0 1
  if (roll < mutationStrength)
    then generate env (Ref.TypeRep @r) constants sizeLeft bound
    else do
      elm2 <- mutated e env (sizeLeft - 1) (Map.insertWith (+) (Ref.SomeTypeRep tr) 1 bound)
      return $ Abstraction tr elm2
mutated stle env@(LambdaEnviroment {constants, mutationStrength}) sizeLeft bound = do
  roll <- uniform 0 1
  if (roll < mutationStrength) then generate env (Ref.TypeRep @r) constants sizeLeft bound else return stle


test :: SimplyTypedLambdaExpression (Bool -> Int -> Int -> Int)
test = Abstraction (Ref.typeRep @(Bool)) (Abstraction (Ref.typeRep @(Int)) (Abstraction (Ref.typeRep @(Int)) (Constant 5)))

generateFromEnv :: forall r. (Typeable r) => LambdaEnviroment r -> RVar (SimplyTypedLambdaExpression r)
generateFromEnv env@(LambdaEnviroment {functions, constants, maxDepth}) = generate env (Ref.TypeRep @r) constants maxDepth (foldr (\(BoundSymbol tr _ _) map -> Map.insertWith (+) (Ref.SomeTypeRep tr) 1 map) Map.empty functions)

generate :: LambdaEnviroment a -> Ref.TypeRep r -> [ConstVal] -> Int -> Bindings -> RVar (SimplyTypedLambdaExpression r)
generate env tr@(Ref.Fun (Ref.TypeRep @a) (Ref.TypeRep @b)) constantTypes sizeLeft bound
  | (sizeLeft > 0) && (Map.member (Ref.SomeTypeRep tr) bound) =  do
      let weight = weights env
      let options = [(application weight, genApplication env tr constantTypes sizeLeft bound), (abstraction weight, genAbstraction env tr constantTypes sizeLeft bound), (variableReference weight, genVariableReference env tr constantTypes sizeLeft bound)]
      expres <- selectWeighted options
      res <- expres
      return res
  | (sizeLeft > 0) =   do
      let weight = weights env
      let options = [(application weight + round (1000 * closestFractionMatch tr bndK), genApplication env tr constantTypes sizeLeft bound), (abstraction weight, genAbstraction env tr constantTypes sizeLeft bound)]
      expres <- selectWeighted options
      res <- expres
      return res
  -- Application can crate a fitting type in a smaller expression. e.g. if':: Bool -> (Int-> Int -> Bool) -> (Int-> Int -> Bool) -> (Int-> Int -> Bool) and target type (Int-> Int -> Bool) -> (Int-> Int -> Bool) -> (Int-> Int -> Bool) can be finished in one Application (if' True::(Int-> Int -> Bool) -> (Int-> Int -> Bool) -> (Int-> Int -> Bool)) and one Var or constant, but resoving it purely with Abstractions would require 5 abstractions and one constant or var
--  | (any (< typeDepth tr) (mapMaybe (sizeMising tr) bndK)) =   do
--      let weight = weights env
--      let options = [(application weight + (typeDepth tr - (minimum (mapMaybe (sizeMising tr) bndK))), genApplication env tr constantTypes sizeLeft bound), (abstraction weight, genAbstraction env tr constantTypes sizeLeft bound)]
--      expres <- selectWeighted options
--      res <- expres
--      return res
  | (Map.member (Ref.SomeTypeRep tr) bound) =   do
      let weight = weights env
      let options = [(abstraction weight, genAbstraction env tr constantTypes sizeLeft bound), (variableReference weight, genVariableReference env tr constantTypes sizeLeft bound)]
      expres <- selectWeighted options
      res <- expres
      return res
  | otherwise =   do
      res <- genAbstraction env tr constantTypes sizeLeft bound
      return res
  where
    bndK = Map.keys bound
generate env tr constantTypes sizeLeft bound
  | (sizeLeft > 0) && (Map.member (Ref.SomeTypeRep tr) bound) =    do
      let weight = weights env
      let options = [(application weight, genApplication env tr constantTypes sizeLeft bound), (constant weight, genConstant tr constantTypes sizeLeft bound), (variableReference weight, genVariableReference env tr constantTypes sizeLeft bound)]
      expres <- selectWeighted options
      res <- expres
      return res
  | (sizeLeft > 0) =    do
      let weight = weights env
      let options = [(application weight + round (1000 * closestFractionMatch tr bndK), genApplication env tr constantTypes sizeLeft bound), (constant weight, genConstant tr constantTypes sizeLeft bound)]
      expres <- selectWeighted options
      res <- expres
      return res
  | (Map.member (Ref.SomeTypeRep tr) bound) =  do
      let weight = weights env
      let options = [(constant weight, genConstant tr constantTypes sizeLeft bound), (variableReference weight, genVariableReference env tr constantTypes sizeLeft bound)]
      expres <- selectWeighted options
      res <- expres
      return res
  | otherwise =   do
      res <- genConstant tr constantTypes sizeLeft bound
      return res
  where
    bndK = Map.keys bound

genVariableReference :: LambdaEnviroment a -> Ref.TypeRep r -> [ConstVal] -> Int -> Bindings -> RVar (SimplyTypedLambdaExpression r)
genVariableReference _ tr@(Ref.TypeRep) _ _ bound = do
  typeIndex <- uniform 0 (((Map.!) bound (Ref.SomeTypeRep tr)) - 1)
  return $ (VariableReference tr typeIndex)

genConstant :: Ref.TypeRep r -> [ConstVal] -> Int -> Bindings -> RVar (SimplyTypedLambdaExpression r)
genConstant (Ref.TypeRep @a) constantTypes _ _ = do
  val <- (constantGen constantTypes) :: RVar (SimplyTypedLambdaExpression a)
  return $ val

constantGen :: forall a. (Typeable a) => [ConstVal] -> RVar (SimplyTypedLambdaExpression a)
constantGen ((ConstVal tr rVal) : rest)
  | Just Ref.HRefl <- Ref.typeRep @a `Ref.eqTypeRep` tr = Constant <$> rVal
  | otherwise = constantGen rest
constantGen [] = error $ "unknown constant " <> show (Ref.typeRep @a)

genAbstraction :: forall r a. LambdaEnviroment a -> Ref.TypeRep r -> [ConstVal] -> Int -> Bindings -> RVar (SimplyTypedLambdaExpression r)
genAbstraction env tr@(Ref.Fun trA@(Ref.TypeRep) trB@(Ref.TypeRep)) constantTypes sizeLeft bound
  | Just Ref.HRefl <- Ref.typeRep @Type `Ref.eqTypeRep` Ref.typeRepKind trA,
    Just Ref.HRefl <- Ref.typeRep @Type `Ref.eqTypeRep` Ref.typeRepKind trB = do
      child <- generate env trB constantTypes (sizeLeft - 1) (Map.insertWith (+) (Ref.SomeTypeRep trA) 1 bound)
      return $ Abstraction trA child
genAbstraction _ tr _ _ _ = error $ "cannot generate Abstraction for " <> show tr

-- generate: e:a = e1:b->a e2:b
-- the by far most complex functions in this module! why?
-- 1. we need to sensibly limit how insane we make b, favorably without excluding anything completely!
-- 2. we need this function to heavily lean towards generating an b->a available in Bindings, so we are likely to use any predefined functions... at all
genApplication :: forall r c a. LambdaEnviroment a -> Ref.TypeRep r -> [ConstVal] -> Int -> Bindings -> RVar (SimplyTypedLambdaExpression r)
genApplication env@(LambdaEnviroment {weights}) tr constantTypes sizeLeft bound
  | (sizeLeft <= 0) = genApplicationClosestToCompletion env tr constantTypes bound
  | otherwise = do
      i <- uniform 0 100
      ( if i < (functionBias weights) && any (1 >) (mapMaybe (matchedFractionS tr) (Map.keys bound))
          then (genApplicationTowardsBound (maximum (filter (1 >) (mapMaybe (matchedFractionS tr) (Map.keys bound)))) env tr constantTypes sizeLeft bound)
          else (genRandomApplication env tr constantTypes sizeLeft bound)
        )

closestFractionMatch :: Ref.TypeRep r -> [Ref.SomeTypeRep] -> Float
closestFractionMatch tr trs | any (1 >) (mapMaybe (matchedFractionS tr) (trs)) = (maximum (filter (1 >) (mapMaybe (matchedFractionS tr) (trs))))
                            | otherwise = 0

genRandomApplication :: forall a r c. LambdaEnviroment a -> Ref.TypeRep r -> [ConstVal] -> Int -> Bindings -> RVar (SimplyTypedLambdaExpression r)
genRandomApplication env tr constantTypes sizeLeft bound = do
  t1 <- randomType constantTypes
  genApplicationWithTypeOfS t1 env tr constantTypes sizeLeft bound

randomType :: [ConstVal] -> RVar Ref.SomeTypeRep
randomType constantTypes = do
  functon :: Int <- (uniform 0 100)
  ret <-
    if functon < 25
      then
        ( do
            tr1 <- randomType constantTypes
            tr2 <- randomType constantTypes
            return (mkFunTy tr1 tr2)
        )
      else
        ( do
            (ConstVal _ (_ :: RVar t1)) <- randomElement constantTypes
            return $ Ref.SomeTypeRep (Ref.TypeRep @t1)
        )
  return ret

genApplicationClosestToCompletion :: forall r a. LambdaEnviroment a -> Ref.TypeRep r -> [ConstVal] -> Bindings -> RVar (SimplyTypedLambdaExpression r)
genApplicationClosestToCompletion env tr constantTypes bound = do
  (ref) <- nextTypeFromClosestBound tr bound
  genApplicationWithTypeOfS ref env tr constantTypes 0 bound

nextTypeFromClosestBound :: Ref.TypeRep r -> Bindings -> RVar Ref.SomeTypeRep
nextTypeFromClosestBound trB bound = randomElement ((getMinimasByMaybe (sizeMising trB) (filter (matchingTypesS trB) (Map.keys bound))))

genApplicationTowardsBound :: forall r c a. Float -> LambdaEnviroment a -> Ref.TypeRep r -> [ConstVal] -> Int -> Bindings -> RVar (SimplyTypedLambdaExpression r)
genApplicationTowardsBound matchedFrac env tr constantTypes sizeLeft bound
  | nextMatchedFrac > 0 = do
      (f :: Float) <- uniform 0 1
      bs <- randomElement (filter (\bs -> Just matchedFrac == matchedFractionS tr bs) (Map.keys bound))
      if (f < matchedFrac + 1) then (genApplicationWithTypeOfS ((nextTypeS tr bs)) env tr constantTypes sizeLeft bound) else (genApplicationTowardsBound nextMatchedFrac env tr constantTypes sizeLeft bound)
  | otherwise = do
      bs <- randomElement (filter (\bs -> Just matchedFrac == matchedFractionS tr bs) (Map.keys bound))
      genApplicationWithTypeOfS ((nextTypeS tr bs)) env tr constantTypes sizeLeft bound
  where
    nextMatchedFrac = (if (any (matchedFrac >) (mapMaybe (matchedFractionS tr) (Map.keys bound))) then (maximum (filter (matchedFrac >) (mapMaybe (matchedFractionS tr) (Map.keys bound)))) else 0) --todo nicer!

-- how many Base types will need to be generated for bound to fit onto tr. This equals the size of the subtree that needs to be generated.
sizeMising :: Ref.TypeRep r -> Ref.SomeTypeRep -> Maybe Int
sizeMising tr (Ref.SomeTypeRep trbs)
  | matchingTypes tr trbs = Just $ (typeDepth tr) - (typeDepth trbs)
  | otherwise = Nothing

matchedFractionS :: Ref.TypeRep r -> Ref.SomeTypeRep -> Maybe Float
matchedFractionS tr (Ref.SomeTypeRep trbs) = matchedFraction tr trbs

matchedFraction :: Ref.TypeRep r -> Ref.TypeRep a -> Maybe Float
matchedFraction tr trbs
  | matchingTypes tr trbs = Just $ fromIntegral (typeDepth trbs) / fromIntegral (typeDepth tr)
  | otherwise = Nothing

nextTypeS :: Ref.TypeRep r -> Ref.SomeTypeRep -> Ref.SomeTypeRep
nextTypeS tr (Ref.SomeTypeRep trbs) = nextType tr trbs

nextType :: Ref.TypeRep r -> Ref.TypeRep a -> Ref.SomeTypeRep
nextType trR@(Ref.Fun (from) (to)) avail
  | Just Ref.HRefl <- (to `Ref.eqTypeRep` avail),
    Just Ref.HRefl <- Ref.typeRep @Type `Ref.eqTypeRep` Ref.typeRepKind from =
      Ref.SomeTypeRep from
  | otherwise = nextType to avail
nextType tra trbs = error ("can't extract nextType from " <> show tra <> " and " <> show trbs)

matchingTypesS :: Ref.TypeRep r -> Ref.SomeTypeRep -> Bool
matchingTypesS tr (Ref.SomeTypeRep trbs) = matchingTypes tr trbs

matchingTypes :: Ref.TypeRep a -> Ref.TypeRep b -> Bool
matchingTypes tra trb | Ref.SomeTypeRep tra == Ref.SomeTypeRep trb = True
matchingTypes (Ref.Fun _ (traRes :: Ref.TypeRep aRes)) trb = matchingTypes (traRes :: Ref.TypeRep aRes) trb
matchingTypes _ _ = False

typeSize :: Ref.TypeRep r -> Int
typeSize (Ref.Fun _ trb) = 1 + (typeSize trb)
typeSize _ = 1

typeDepth :: Ref.TypeRep r -> Int
typeDepth (Ref.Fun _ (trb :: Ref.TypeRep b)) = 1 + (typeDepth (trb :: Ref.TypeRep b))
typeDepth _ = 1

genApplicationWithTypeOfS :: forall r a. Ref.SomeTypeRep -> LambdaEnviroment a -> Ref.TypeRep r -> [ConstVal] -> Int -> Bindings -> RVar (SimplyTypedLambdaExpression r)
genApplicationWithTypeOfS (Ref.SomeTypeRep btr@(Ref.TypeRep))
  | Just Ref.HRefl <- Ref.typeRep @Type `Ref.eqTypeRep` Ref.typeRepKind btr = genApplicationWithTypeOfB btr
genApplicationWithTypeOfS (Ref.SomeTypeRep btr) = error $ "typeRepKind not Type: " <> show (Ref.typeRepKind btr)

genApplicationWithTypeOfB :: forall r a (b :: Type). Ref.TypeRep b -> LambdaEnviroment a -> Ref.TypeRep r -> [ConstVal] -> Int -> Bindings -> RVar (SimplyTypedLambdaExpression r)
genApplicationWithTypeOfB trB@(Ref.TypeRep) env trR@(Ref.TypeRep) constantTypes sizeLeft bound = do
  sizeDistribution <- uniform 0 (sizeLeft - 1)
  right <- generate env trB constantTypes sizeDistribution bound
  left <- generate env (Ref.Fun (Ref.TypeRep @b) trR) constantTypes ((sizeLeft - 1) - sizeDistribution) bound
  return $ Application left right

selectWeighted :: [(Int, a)] -> RVar a
selectWeighted x = do
  let total = Protolude.sum (map fst x)
  selection <- uniform 1 total
  return $ selectAtWeight selection (NE.fromList x)

selectAtWeight :: Int -> NonEmpty (Int, a) -> a
selectAtWeight _ (x :| []) = snd x
selectAtWeight w (x :| xs)
  | fst x >= w = snd x
  | otherwise = selectAtWeight (w - fst x) (NE.fromList xs)

eval :: [BoundSymbol] -> SimplyTypedLambdaExpression ex -> ex
eval bound (Abstraction rep stle) = lam bound rep stle
eval bound (Application stleAtoB stleA) = (eval bound stleAtoB) (eval bound stleA)
eval bound (VariableReference rep inx) = (getSymbolsOfType bound rep) !! inx
eval _ (Constant res) = res

lam :: [BoundSymbol] -> Ref.TypeRep a -> SimplyTypedLambdaExpression (b) -> (a -> b)
lam bound Ref.TypeRep stle = \(aVal :: a) -> eval (appendToBoundVar bound aVal) stle

appendToBoundVar :: (Typeable a) => [BoundSymbol] -> a -> [BoundSymbol]
appendToBoundVar bv val = bv ++ [BoundSymbol (Ref.typeOf val) val Nothing]

listAppend :: (Typeable a) => a -> Maybe [Dynamic] -> Maybe [Dynamic]
listAppend val (Just dyns) = Just (dyns ++ [toDyn val])
listAppend val (Nothing) = Just [toDyn val]

getMinimasBy :: (Ord b) => (a -> b) -> [a] -> [a]
getMinimasBy fun as = filter (\a -> fun a == minOverAs) as
  where
    minOverAs = minimum (map fun as)

getMinimasByMaybe :: (Ord b) => (a -> Maybe b) -> [a] -> [a]
getMinimasByMaybe fun as = filter (\a -> fun a == Just minOverAs) as
  where
    minOverAs = minimum (mapMaybe fun as)

getMaximasBy :: (Ord b) => (a -> b) -> [a] -> [a]
getMaximasBy fun as = filter (\a -> fun a == maxOverAs) as
  where
    maxOverAs = maximum (map fun as)

getMaximasByMaybe :: (Ord b) => (a -> Maybe b) -> [a] -> [a]
getMaximasByMaybe fun as = filter (\a -> fun a == Just maxOverAs) as
  where
    maxOverAs = maximum (mapMaybe fun as)
