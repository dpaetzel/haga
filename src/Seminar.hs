{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Seminar where

import Data.List ((!!), (\\), lookup, zipWith3)
import Data.List.Extra (delete, nubOrd, nubOrdOn)
import Data.Random
import qualified Data.Text as T
import GA
import Pretty
import Protolude
import Test.QuickCheck hiding (sample, shuffle)
import Test.QuickCheck.Monadic (assert, monadicIO)

instance Pretty Text where
  pretty = identity

type Student = Text

type Topic = Text

instance Pretty (Maybe Topic) where
  pretty (Just t) = t
  pretty Nothing = "Kein Thema"

newtype Priorities = P {unP :: [(Student, [(Topic, Int)])]}
  deriving (Eq, Show)

instance Arbitrary Priorities where
  arbitrary = priorities <$> Test.QuickCheck.arbitrary

{-|
Smart constructor for priority lists.

Completes a priority list, that is, if students did not assign priorities to
certain topics, adds these topics to their respective priority lists having a
priority of one less than the lowest priority assigned by them.

In addition, throws out all but the first occurence of each topic in a student's
list (i.e. removes assignments of multiple priorities to one topic for each
student) as well as all but the first occurrence of each student.
-}
priorities :: [(Student, [(Topic, Int)])] -> Priorities
priorities p = P . nubOrdOn fst $ second priorities1 <$> p
  where
    priorities1 :: [(Topic, Int)] -> [(Topic, Int)]
    priorities1 [] =
      topics p `zip` repeat 1
    priorities1 ps =
      let tLacking = topics p \\ (fst <$> ps) :: [Topic]
          pWorst = maximum (snd <$> ps) + 1 :: Int
       in nubOrdOn fst $ ps ++ (tLacking `zip` repeat pWorst)
    topics = nubOrd . concatMap (fmap fst . snd)

prop_priorities_allListsSameLength :: [(Student, [(Topic, Int)])] -> Bool
prop_priorities_allListsSameLength p =
  case unP . priorities $ p of
    [] -> True
    (s : ss) -> all (((length . snd) s ==) . length . snd) ss

{-|
The students that assigned priorities to topics.
-}
students :: Priorities -> [Student]
students = fmap fst . unP

{-|
The topics students assigned priorities to.

Since 'Priorities' objects are well-formed due to the smart constructor, we can
simply return the topics the first student assigned priorities to.
-}
topics :: Priorities -> [Topic]
topics (P []) = []
topics (P (s : _)) = fmap fst . snd $ s

{-|
The priority value given by a student to a topic.
-}
prioOf :: Priorities -> Student -> Topic -> Int
prioOf p s t = fromMaybe (lowestPriority p + 1) $ lookup s (unP p) >>= lookup t

prop_prioOf_empty :: Bool
prop_prioOf_empty = prioOf (P []) "S" "T" == 1

prop_prioOf_singletonFound :: Bool
prop_prioOf_singletonFound =
  prioOf (P [("S", [("Existing topic", 10)])]) "S" "Existing topic" == 10

prop_prioOf_singletonNotFound :: Bool
prop_prioOf_singletonNotFound =
  prioOf (P [("S", [("Existing topic", 10)])]) "S" "Non-existing topic" == 11

{-|
The lowest priority assigned by a student to a topic.
-}
lowestPriority :: Priorities -> Int
lowestPriority = fromMaybe 0 . maximumMay . fmap snd . join . fmap snd . unP

type Assignment = [(Student, Maybe Topic)]

data I = I Priorities Assignment
  deriving (Eq, Show)

instance Pretty I where
  pretty (I p a) =
    T.unlines (gene <$> a)
    where
      gene :: (Student, Maybe Topic) -> Text
      gene (s, t) =
        pretty s <> ": " <> pretty t <> prio s t
      prio :: Student -> Maybe Topic -> Text
      prio s t = " (" <> show (prioOf' p s t) <> ")"

{-|
The priority value given by a student to a topic including the case of her not
receiving a topic.
-}
prioOf' :: Priorities -> Student -> Maybe Topic -> Int
prioOf' p _ Nothing = lowestPriority p + 2
prioOf' p s (Just t) = prioOf p s t

instance Individual I where

  new (I p _) =
    sample $ I p . zip (nubOrd $ students p) <$> shuffle topics'
    where
      topics' = (Just <$> topics p) ++ padding
      padding = replicate (length (students p) - length (topics p)) Nothing

  fitness (I p a) =
    return . negate . sum
      $ fromIntegral . uncurry (prioOf' p) <$> a

  mutate (I p a) = do
    x <- sample $ Uniform 0 (length a - 1)
    y <- sample $ Uniform 0 (length a - 1)
    return . I p $ switch x y a

  {-|
  Borrowed from TSP: Crossover cuts the parents in two and swaps them (if this
  does not create an invalid offspring).
  
  TODO Assumes that both individuals are based on the same priorities.
  -}
  crossover1 (I p a1) (I _ a2) = do
    let l = fromIntegral $ min (length a1) (length a2) :: Double
    x <- sample $ Uniform 0 l
    let a1' = zipWith3 (f x) a1 a2 [0 ..]
    let a2' = zipWith3 (f x) a2 a1 [0 ..]
    if valid p a1' && valid p a2'
      then return . Just $ (I p a1', I p a2')
      else return Nothing
    where
      f x v1 v2 i = if i <= x then v1 else v2

{-|
Swaps topics at positions 'i'' and 'j'' in the given assignment.
-}
switch :: Int -> Int -> Assignment -> Assignment
switch i' j' xs
  | i' == j' = xs
  | 0 <= i' && i' < length xs && 0 <= j' && j' < length xs =
    let i = min i' j'
        j = max i' j'
        ei = xs !! i
        ej = xs !! j
        left = take i xs
        middle = take (j - i - 1) $ drop (i + 1) xs
        right = drop (j + 1) xs
     in left ++ [(fst ei, snd ej)] ++ middle ++ [(fst ej, snd ei)] ++ right
  | otherwise = xs

{-|
Whether the given assignment is valid (every student occurs at most once, as
does every topic; also, there is only no topic given to students if there are
less topics than students).

Assumes that the priorities are well-formed.
-}
valid :: Priorities -> Assignment -> Bool
valid p a =
  -- all students must be part of the solution
  sort (students p) == sort studentsAssigned
    -- each actual topic (i.e. not “no topic”) is assigned at most once
    && nubOrd (delete Nothing topicsAssigned) == delete Nothing topicsAssigned
  where
    studentsAssigned = fmap fst a
    topicsAssigned = fmap snd a

prop_new_valid :: Priorities -> Property
prop_new_valid p = monadicIO $ do
  I _ a <- lift $ new (I p [])
  assert $ valid p a

prop_mutate_valid :: Priorities -> Property
prop_mutate_valid p = monadicIO $ do
  a <- lift . new $ I p []
  I _ a <- lift $ mutate a
  assert $ valid p a

prop_crossover1_valid :: Priorities -> Property
prop_crossover1_valid p = monadicIO $ do
  a1 <- lift . new $ I p []
  a2 <- lift . new $ I p []
  asM <- lift $ crossover1 a1 a2
  assert
    $ case asM of
      Just (I _ a1', I _ a2') -> valid p a1' && valid p a2'
      Nothing -> True

{-|
Generator for lists fulfilling 'unique', that is, only containing unique
elements.
-}
noDupsList :: (Arbitrary a, Eq a, Ord a) => Gen [a]
noDupsList = nubOrd <$> arbitrary

prop_noDupsList :: Property
prop_noDupsList = forAll (noDupsList :: Gen [Int]) unique

{-|
Whether the given list only contains unique elements.
-}
unique :: (Ord a) => [a] -> Bool
unique xs = length xs == (length . nubOrd) xs

return []

runTests :: IO Bool
runTests = $quickCheckAll
