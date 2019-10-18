{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Seminar where

import Data.List ((!!), lookup, nub, zipWith3)
import Data.Random
import Data.Random.Distribution.Uniform
import qualified Data.Text as T
import GA
import Pretty
import Protolude
import Test.QuickCheck hiding (sample, shuffle)
import Test.QuickCheck.Instances

data Name
  = N
      { firstName :: Text,
        lastName :: Text
      }
  deriving (Eq, Show)

instance Arbitrary Name where
  arbitrary = N <$> arbitrary <*> arbitrary

instance Pretty Name where
  pretty (N f l) = f <> " " <> l

type EMail = Text

data Student
  = S
      { name :: Name,
        email :: EMail
      }
  deriving (Eq, Show)

instance Arbitrary Student where
  arbitrary = S <$> arbitrary <*> arbitrary

instance Pretty Student where
  pretty (S n e) = pretty n <> " <" <> e <> ">"

{-|
The seminar GA is about giving topics to students. If there are not enough
topics, some students might get assigned 'NoTopic'.
-}
data T
  = T Text
  | NoT
  deriving (Show)

{-|
'Topic' is not 'Maybe' because this 'Eq' instance is different ('Nothing' @==@
'Nothing' but 'NoTopic' @/=@ 'NoTopic').
-}
instance Eq T where
  T x == T y = x == y
  -- NoT == _ = False
  -- _ == NoT = False
  NoT == NoT = True
  NoT == _ = False
  _ == NoT = False

instance Arbitrary T where
  arbitrary = oneof [T <$> arbitrary, return NoT]

instance Pretty T where
  pretty (T s) = s
  pretty NoT = "Kein Thema"

topicToMaybe (T x) = Just x
topicToMaybe NoT = Nothing

type Priorities = [(Student, [(T, Int)])]

students :: Priorities -> [Student]
students = fmap fst

topics :: Priorities -> [T]
topics p = topics' ++ padding
  where
    padding = replicate (length (students p) - length topics') NoT
    topics' = nub . join $ fmap fst . snd <$> p

{-|
The priority of topics that have not been assigned a priority is the sum of all
priorities given by all the students. The priority of 'NoTopic' is one more than
that value.
-}
prioOf :: Priorities -> Student -> T -> Int
prioOf p _ NoT = (1 +) . sum $ (sum . fmap snd) . snd <$> p
prioOf p s t = maybe (prioOf p s NoT) identity $ lookup s p >>= lookup t

type Assignment = [(Student, T)]

data I = I Priorities Assignment
  deriving (Eq, Show)

instance Pretty I where
  pretty i@(I p a) =
    T.unlines (gene <$> a)
    where
      gene :: (Student, T) -> Text
      gene (s, t) =
        pretty s <> ": " <> pretty t <> prio s t
      prio :: Student -> T -> Text
      prio s t = " (" <> show (prioOf p s t) <> ")"

instance Individual I where

  new (I p _) =
    sample $ I p . zip (nub $ students p) <$> shuffle (nub $ topics p)

  fitness (I p a) =
    return . (fromIntegral (prioOf p undefined NoT) /) . sum
      $ fromIntegral . uncurry (prioOf p) <$> a

  mutate (I p a) = do
    x <- sample $ Uniform 0 (length a - 1)
    y <- sample $ Uniform 0 (length a - 1)
    return . I p $ switch x y a

  {-|
  Borrowed from TSP: Crossover cuts the parents in two and swaps them (if this
  does not create an invalid offspring).
  
  TODO Assumes that both individuals are based on the same priorities.
  
  TODO Require type-wise that both Assignments are of the same length.
  -}
  crossover1 (I p a1) (I _ a2) = do
    let l = fromIntegral $ min (length a1) (length a2) :: Double
    x <- sample $ Uniform 0 l
    let a1' = zipWith3 (f x) a1 a2 [0 ..]
    let a2' = zipWith3 (f x) a2 a1 [0 ..]
    if valid a1' && valid a2'
      then return . Just $ (I p a1', I p a2')
      else return Nothing
    where
      f x v1 v2 i = if i <= x then v1 else v2

switch :: Int -> Int -> Assignment -> Assignment
switch i' j' xs
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

valid :: Assignment -> Bool
valid a =
  length a == length (nub $ fst <$> a)
    && length a == length (nub $ snd <$> a)