{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module LambdaCalculusV2 where

import qualified Data.Map.Strict as Map
import Data.Kind
import qualified Type.Reflection as Ref
import Data.Dynamic
import Protolude
import Protolude.Partial
import Protolude.Error
import Data.Random

type BoundVars = Map.Map (Ref.SomeTypeRep) [Dynamic]

-- We specify a and use GADTs to allow Haskell to guarantee full type safety over these expressions!
-- This gurantees us that a SimplyTypedLambdaExpression t describes a lambda expression of type a!
data SimplyTypedLambdaExpression t where
    Application ::  (Typeable a, Typeable b) => SimplyTypedLambdaExpression (a -> b) -> SimplyTypedLambdaExpression a -> SimplyTypedLambdaExpression b  -- e = e1 e2
    Abstraction ::  Ref.TypeRep a -> SimplyTypedLambdaExpression (b) -> SimplyTypedLambdaExpression (a -> b)                -- e = λx:a. e
    VariableReference :: Typeable a => Ref.TypeRep a -> Int -> SimplyTypedLambdaExpression a                                -- e = x
    Constant ::  (Typeable a, Ord a, Hashable a ) => a -> SimplyTypedLambdaExpression a                                                  -- e = c this Includes predefined function use!

instance Eq (SimplyTypedLambdaExpression t) where
    e1 == e2 = compare e1 e2 == EQ

instance Ord (SimplyTypedLambdaExpression t) where
    compare (Application  (stleAtoB1 :: SimplyTypedLambdaExpression (a1->t)) (stleA1 :: SimplyTypedLambdaExpression a1)) (Application (stleAtoB2 :: SimplyTypedLambdaExpression (a2->t)) (stleA2 :: SimplyTypedLambdaExpression a2)) = case eqT @a1 @a2 of
        Just Refl ->  (compare stleAtoB1 stleAtoB2) `thenCmp` (compare stleA1 stleA2)
        _ -> compare ( Ref.SomeTypeRep (Ref.TypeRep @a1)) ( Ref.SomeTypeRep (Ref.TypeRep @a2))
    compare (Abstraction rep1 stle1) (Abstraction rep2 stle2) =  (compare rep1 rep2)  `thenCmp` (compare stle1 stle2)
    compare (VariableReference repA inx1) (VariableReference repB inx2) =  (compare repA repB) `thenCmp` (compare inx1 inx2)
    compare (Constant res1) (Constant res2) = compare res1 res2
    compare (Application  _ _) _ = LT
    compare _ (Application _  _) = GT
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
thenCmp o1 _  = o1

class ConstVal a where
    randomValue :: a -> RVar a


test :: SimplyTypedLambdaExpression (Bool -> Int -> Int -> Int)
test = Abstraction (Ref.typeRep @(Bool)) (Abstraction (Ref.typeRep @(Int)) (Abstraction (Ref.typeRep @(Int)) (Constant 5)))

generate :: ConstVal c => Ref.TypeRep r -> [c] -> Int -> BoundVars -> RVar (SimplyTypedLambdaExpression r)
generate targetType constantTypes sizeLeft bound = undefined

eval :: BoundVars -> SimplyTypedLambdaExpression ex -> ex
eval bound (Abstraction rep stle) = lam bound rep stle
eval bound (Application stleAtoB stleA) = (eval bound stleAtoB) (eval bound stleA)
eval bound (VariableReference rep inx) = fromDyn ( (bound Map.! (Ref.SomeTypeRep rep)) !! inx) (error ("we couldn't find " <> (show rep) <> " in our boundVars: " <> (show bound)))
eval _ (Constant res) = res


lam :: BoundVars -> Ref.TypeRep a -> SimplyTypedLambdaExpression (b) -> (a -> b)
lam bound Ref.TypeRep stle = \(aVal :: a) -> eval (appendToBoundVar bound aVal) stle

appendToBoundVar :: (Typeable a) => BoundVars -> a -> BoundVars
appendToBoundVar bv val = Map.alter (listAppend val) (Ref.SomeTypeRep (Ref.typeOf val)) bv


listAppend::(Typeable a) => a -> Maybe [Dynamic] -> Maybe [Dynamic]
listAppend val (Just dyns) = Just (dyns ++ [toDyn val])
listAppend val (Nothing) = Just [toDyn val]
