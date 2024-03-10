{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Seminar where

import Data.List (lookup, zipWith3, (!!), (\\))
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
  -- instance Pretty (Maybe Student) where
  pretty (Just t) = t
  pretty Nothing = "Kein"

newtype Priorities = P {unP :: [(Student, [(Topic, Int)])]}
  deriving (Eq, Show)

instance Arbitrary Priorities where
  arbitrary = priorities <$> Test.QuickCheck.arbitrary

-- |
-- Smart constructor for priority lists.
--
-- Completes a priority list, that is, if students did not assign priorities to
-- certain topics, adds these topics to their respective priority lists having a
-- priority of one less than the lowest priority assigned by them.
--
-- In addition, throws out all but the first occurence of each topic in a student's
-- list (i.e. removes assignments of multiple priorities to one topic for each
-- student) as well as all but the first occurrence of each student.
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

-- |
-- The students that assigned priorities to topics.
students :: Priorities -> [Student]
students = fmap fst . unP

-- |
-- The topics students assigned priorities to.
--
-- Since 'Priorities' objects are well-formed due to the smart constructor, we can
-- simply return the topics the first student assigned priorities to.
topics :: Priorities -> [Topic]
topics (P []) = []
topics (P (s : _)) = fmap fst . snd $ s

-- |
-- The priority value given by a student to a topic.
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

-- |
-- The lowest priority assigned by a student to a topic.
lowestPriority :: Priorities -> Int
lowestPriority = fromMaybe 0 . maximumMay . fmap snd . join . fmap snd . unP

type Assignment = [(Maybe Student, Maybe  Topic)]

instance Individual Assignment

newtype AssignmentEnviroment = AssignmentEnviroment ([Student],[Topic]) deriving Eq

instance Pretty AssignmentEnviroment where
  pretty (AssignmentEnviroment (persons,assignables)) = "Persons: " <> show persons <> " Assignables: " <> show assignables

instance Environment Assignment AssignmentEnviroment where
  new (AssignmentEnviroment (persons,assignables)) = do
    let aPadding = replicate (length persons - length assignables) Nothing
    let paddedAssignables = (Just <$> assignables) ++ aPadding
    let pPadding = replicate (length assignables - length persons) Nothing
    let paddedPersons = (Just <$> persons) ++ pPadding

    mixedAssignables <- shuffle paddedAssignables
    return $ zip paddedPersons mixedAssignables

  nX _ = 1

  mutate _ assignment = do
    x <- uniform 0 (length assignment - 1)
    y <- uniform 0 (length assignment - 1)
    return $ switch x y assignment

  -- \|
  --  Borrowed from TSP: Crossover cuts the parents in two and swaps them (if this
  --  does not create an invalid offspring).
  --
  --  TODO Assumes that both individuals are based on the same priorities.
  --
  crossover1 e assignment1 assignment2 = do
    let l = fromIntegral $ min (length assignment1) (length assignment2) :: Double
    x <- uniform 0 l
    let assignment1' = zipWith3 (f x) assignment1 assignment2 [0 ..]
    let assignment2' = zipWith3 (f x) assignment2 assignment1 [0 ..]
    if valid e assignment1' && valid e assignment2'
      then return . Just $ ( assignment1', assignment2')
      else return Nothing
    where
      f x v1 v2 i = if i <= x then v1 else v2


instance Pretty Assignment where
  pretty (a) =
    T.unlines (gene <$> a)
    where
      gene :: (Maybe Student, Maybe Topic) -> Text
      gene (s, t) =
        pretty s <> ": " <> pretty t

-- |
-- The priority value given by a student to a topic including the case of her not
-- receiving a topic.
prioOf' :: Priorities -> Maybe Student -> Maybe Topic -> Int
-- TODO Maybe make this neutral?
prioOf' p Nothing Nothing = lowestPriority p + 2
prioOf' p (Just _) Nothing = lowestPriority p + 2
prioOf' p Nothing (Just _) = lowestPriority p + 2
prioOf' p (Just s) (Just t) = prioOf p s t

instance Evaluator Assignment Priorities R where
  fitness' prio assment =
    negate . sum $ fromIntegral . uncurry (prioOf' prio) <$> assment

-- |
-- Swaps topics at positions 'i'' and 'j'' in the given assignment.
switch :: Int -> Int -> Assignment -> Assignment
switch i' j' xs
  | i' == j' = xs
  | 0 <= i' && i' < length xs && 0 <= j' && j' < length xs =
    zipWith (\ind y ->
    if ind == i' then (fst y, snd (xs !! j'))
    else if ind == j' then (fst y, snd (xs !! i'))
    else y) [0..] xs
  | otherwise = xs

-- |
-- Whether the given assignment is valid (every student occurs at most once, as
-- does every topic; also, there is only no topic given to students if there are
-- less topics than students).
--
-- Assumes that the priorities are well-formed.
valid :: AssignmentEnviroment -> Assignment -> Bool
valid (AssignmentEnviroment (persons,assignables)) a =
  -- all students must be part of the solution
  sort (persons) == (catMaybes $ sort studentsAssigned)
    -- each actual topic (i.e. not “no topic”) is assigned at most once
    && nubOrd (delete Nothing topicsAssigned) == delete Nothing topicsAssigned
  where
    studentsAssigned = fmap fst a
    topicsAssigned = fmap snd a

-- prop_new_valid :: Priorities -> Property
-- prop_new_valid p = monadicIO $ do
--   I _ a <- lift $ new (I p [])
--   assert $ valid p a

-- prop_mutate_valid :: Priorities -> Property
-- prop_mutate_valid p = monadicIO $ do
--   a <- lift . new $ I p []
--   I _ a <- lift $ mutate a
--   assert $ valid p a

-- prop_crossover1_valid :: Priorities -> Property
-- prop_crossover1_valid p = monadicIO $ do
--   a1 <- lift . new $ I p []
--   a2 <- lift . new $ I p []
--   asM <- lift $ crossover1 a1 a2
--   assert
--     $ case asM of
--       Just (I _ a1', I _ a2') -> valid p a1' && valid p a2'
--       Nothing -> True

-- |
-- Generator for lists fulfilling 'unique', that is, only containing unique
-- elements.
noDupsList :: (Arbitrary a, Eq a, Ord a) => Gen [a]
noDupsList = nubOrd <$> arbitrary

prop_noDupsList :: Property
prop_noDupsList = forAll (noDupsList :: Gen [Int]) unique

-- |
-- Whether the given list only contains unique elements.
unique :: (Ord a) => [a] -> Bool
unique xs = length xs == (length . nubOrd) xs

return []

runTests :: IO Bool
runTests = $quickCheckAll
