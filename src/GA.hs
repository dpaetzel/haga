
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : GA
-- Description : Abstract genetic algorithm
-- Copyright   : David Pätzel, 2019
-- License     : GPL-3
-- Maintainer  : David Pätzel <david.paetzel@posteo.de>
-- Stability   : experimental
--
-- Simplistic abstract definition of a genetic algorithm.
--
-- In order to use it for a certain problem, basically, you have to make your
-- solution type an instance of 'Individual' and then simply call the 'run'
-- function.
module GA ( Environment,new, population, mutate, crossover1,crossover, Evaluator, fitness,  Individual, GA.run, tournament, N, R, Population, steps, bests, runTests) where

import Control.Arrow hiding (first, second)
import Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NE
import qualified Data.List.NonEmpty.Extra as NE (appendl)
import Data.Random
import System.Random.MWC (create)
import Pipes
import Protolude
import Pretty
import Test.QuickCheck hiding (sample, shuffle)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic

-- TODO there should be a few 'shuffle's here

-- TODO enforce this being > 0
type N = Int

type R = Double


-- |
--  An Environment that Individuals of type i can be created from
--  It stores all information required to create and change Individuals correctly
--
class (Pretty e, Individual i) => Environment i e where
  -- |
  --  Generates a completely random individual.
  --
  new :: e -> RVar i

  -- |
  --  Generates a random population of the given size.
  population :: e -> N -> RVar (Population i)
  population env n
    | n <= 0 = undefined
    | otherwise = NE.fromList <$> replicateM n (new env)

  mutate :: e -> i -> RVar i

  crossover1 :: e -> i -> i -> RVar (Maybe (i, i))


  -- |
  --  Performs an n-point crossover.
  --
  --  Given the function for single-point crossover, 'crossover1', this function can
  --  be derived through recursion and a monad combinator (which is also the default
  --  implementation).
  crossover :: e -> N -> i -> i -> RVar (Maybe (i, i))
  crossover env n i1 i2
    | n <= 0 = return $ Just (i1, i2)
    | otherwise = do
        isM <- crossover1 env i1 i2
        maybe (return Nothing) (uncurry (crossover env (n - 1))) isM

-- |
--  An Evaluator that Individuals of type i can be evaluated by
--  It stores all information required to evaluate an individuals fitness
--
class (Eq e, Individual i) => Evaluator i e where
  -- |
  --  An individual's fitness. Higher values are considered “better”.
  --
  --  We explicitely allow fitness values to be have any sign (see, for example,
  --  'proportionate1').
  fitness :: e -> i -> R

class (Pretty i, Eq i) => Individual i



-- |
-- Populations are just basic non-empty lists.
type Population i = NonEmpty i

-- |
-- Produces offspring circularly from the given list of parents.
children ::
  (Individual i, Environment i e) =>
  e ->
  -- | The @nX@ of the @nX@-point crossover operator
  N ->
  NonEmpty i ->
  RVar (NonEmpty i)
children e _ (i :| []) = (:| []) <$> mutate e i
children e nX (i1 :| [i2]) = children2 e nX i1 i2
children e nX (i1 :| i2 : is') =
  (<>) <$> children2 e nX i1 i2 <*> children e nX (NE.fromList is')


children2 :: (Individual i, Environment i e) => e -> N -> i -> i -> RVar (NonEmpty i)
children2 e nX i1 i2 = do
  -- TODO Add crossover probability?
  (i3, i4) <- fromMaybe (i1, i2) <$> crossover e nX i1 i2
  i5 <- mutate e i3
  i6 <- mutate e i4
  return $ i5 :| [i6]

-- |
-- The best according to a function; returns up to @k@ results and the remaining
-- population.
--
-- If @k <= 0@, this returns the best one anyway (as if @k == 1@).
bestsBy ::
  (Individual i) =>
  N ->
  (i -> R) ->
  Population i ->
  (NonEmpty i, [i])
bestsBy k f pop
  | k <= 0 = bestsBy 1 f pop
  | otherwise = let (elites, rest) = NE.splitAt k $ map fst $ NE.sortBy (comparing (Down . snd)) $ map (\i -> (i, f i)) pop
                in (NE.fromList elites, rest)

-- |
-- The @k@ best individuals in the population when comparing using the supplied
-- function.
bestsBy' :: (Individual i) => N -> (i -> R) -> Population i -> [i]
bestsBy' k f pop
  | k <= 0 = bestsBy' 1 f pop
  | otherwise = NE.take k $ map fst $ NE.sortBy (comparing (Down . snd)) $ map (\i -> (i, f i)) pop


-- |
-- The @k@ worst individuals in the population (and the rest of the population).
worst :: (Individual i, Evaluator i e) => e -> N -> Population i -> (NonEmpty i, [i])
worst e k = bestsBy k (negate . fitness e)

-- |
-- The @k@ best individuals in the population (and the rest of the population).
bests :: (Individual i, Evaluator i e) => e -> N -> Population i -> (NonEmpty i, [i])
bests e k = bestsBy k (fitness e)

-- TODO add top x percent parent selection (select n guys, sort by fitness first)

-- |
-- Performs one iteration of a steady state genetic algorithm that in each
-- iteration that creates @k@ offspring simply deletes the worst @k@ individuals
-- while making sure that the given percentage of elitists survive (at least 1
-- elitist, even if the percentage is 0 or low enough for rounding to result in 0
-- elitists).
stepSteady ::
  (Individual i, Evaluator i eval, Environment i env ) =>
  eval ->
  env ->
  -- | Mechanism for selecting parents
  Selection RVar i ->
  -- | Number of parents @nParents@ for creating @nParents@ children
  N ->
  -- | How many crossover points (the @nX@ in @nX@-point crossover)
  N ->
  -- | Elitism ratio @pElite@
  R ->
  Population i ->
  RVar (Population i)
stepSteady eval env select nParents nX pElite pop = do
  -- TODO Consider keeping the fitness evaluations already done for pop (so we
  -- only reevaluate iChildren)
  iParents <- select nParents pop
  iChildren <- NE.filter (`notElem` pop) <$> children env nX iParents
  let pop' = pop `NE.appendl` iChildren
  let eliteSize = floor . (pElite *) . fromIntegral $ NE.length pop
  let (elitists, rest) = bests eval eliteSize pop'
  case rest of
    [] -> return elitists
    _notEmpty ->
      -- NOTE 'bests' always returns at least one individual, thus we need this
      -- slightly ugly branching
      if length elitists == length pop
        then return elitists
        else
          return $ elitists <> (fst $ bests eval (length pop - length elitists) (NE.fromList rest))

-- |
-- Given an Enviroment and Evaluator, runs the GA until the termination criterion is
-- fulfilled.
--
-- Uses the pipes library to, in each step, 'Pipes.yield' the currently best known
-- solution.
run ::
  (Individual i, Evaluator i eval, Environment i env ) =>
  eval ->
  env ->
  -- | Mechanism for selecting parents
  Selection RVar i ->
  -- | Number of parents @nParents@ for creating @nParents@ children
  N ->
  -- | How many crossover points (the @nX@ in @nX@-point crossover)
  N ->
  -- | Elitism ratio @pElite@
  R ->
  -- | Population size
  N ->
  Termination i ->
  Producer (Int, R) IO (Population i)
run eval env select nParents nX pElite nPop term = do
        mwc <- lift create
        let x = \currPop generation -> do
              currPop' <- lift $ sampleFrom mwc $ currPop
              if term currPop' generation
                then return currPop'
                else do
                  let nextPop = stepSteady eval env select nParents nX pElite currPop'
                  let fBest = fitness eval $ NE.head $ fst $ bests eval 1 currPop'
                  Pipes.yield (generation, fBest)
                  x nextPop (generation + 1)
        x (population env nPop) 0


-- * Selection mechanisms

-- |
-- A function generating a monadic action which selects a given number of
-- individuals from the given population.
type Selection m i = N -> Population i -> m (NonEmpty i)

-- |
-- Selects @n@ individuals from the population the given mechanism by repeatedly
-- selecting a single individual using the given selection mechanism (with
-- replacement, so the same individual can be selected multiple times).
chain ::
  (Individual i) =>
  (Population i -> RVar i) ->
  Selection RVar i
-- TODO Ensure that the same individual is not selected multiple times
-- (require Selections to partition)
chain select1 n pop
  | n > 1 = (<|) <$> select1 pop <*> chain select1 (n - 1) pop
  | otherwise = (:|) <$> select1 pop <*> return []

-- |
-- Selects @n@ individuals from the population by repeatedly selecting a single
-- indidual using a tournament of the given size (the same individual can be
-- selected multiple times, see 'chain').
tournament :: (Individual i, Evaluator i e) => e -> N -> Selection RVar i
tournament eval nTrnmnt = chain (tournament1 eval nTrnmnt)

-- |
-- Selects one individual from the population using tournament selection.
tournament1 ::
  (Individual i, Evaluator i e) =>
  e ->
  -- | Tournament size
  N ->
  Population i ->
  RVar i
tournament1 eval nTrnmnt pop
  -- TODO Use Positive for this constraint
  | nTrnmnt <= 0 = undefined
  | otherwise = do
        paricipants <- withoutReplacement nTrnmnt pop
        return $ NE.head $ fst $ bests eval 1 paricipants

-- |
-- Selects @n@ individuals uniformly at random from the population (without
-- replacement, so if @n >= length pop@, simply returns @pop@).
withoutReplacement ::
  -- | How many individuals to select
  N ->
  Population i ->
  RVar (NonEmpty i)
withoutReplacement 0 _ = undefined
withoutReplacement n pop
  | n >= length pop = return pop
  | otherwise = fmap (NE.fromList) (shuffleNofM n (length pop) (NE.toList pop))

-- * Termination criteria

-- |
-- Termination decisions may take into account the current population and the
-- current iteration number.
type Termination i = Population i -> N -> Bool

-- |
-- Termination after a number of steps.
steps :: N -> Termination i
steps tEnd _ t = t >= tEnd

-- * Helper functions

-- |
-- Shuffles a non-empty list.
shuffle' :: NonEmpty a -> RVar (NonEmpty a)
shuffle' xs@(_ :| []) = return xs
shuffle' xs = fmap (NE.fromList) (shuffle (toList xs))



instance Pretty Integer where
  pretty i = "Found int: " <> show i

instance Individual Integer

newtype IntTestEnviroment = IntTestEnviroment ((Integer,Integer),Integer) deriving (Eq) -- IntTestEnviroment ((0,100000),10)

instance Pretty IntTestEnviroment where
  -- instance Pretty (Maybe Student) where
  pretty (IntTestEnviroment ((i,j),k)) = "IntTestEnviroment of Individuals between " <> (show i) <> " and "  <> (show j) <> " variance when mutating is " <> (show k)


instance Environment Integer IntTestEnviroment where

  new (IntTestEnviroment ((from,to),_)) = uniform from to

  mutate (IntTestEnviroment ((from,to),wiggle)) i = uniform (max from (i - wiggle)) (min to (i + wiggle))

  crossover1 _ i1 i2 = do
    i1' <- uniform i1 i2
    i2' <- uniform i1 i2
    return $ Just (i1',i2')

data NoData = NoData deriving (Eq)

instance Evaluator Integer NoData  where

  fitness _ = fromIntegral . negate

prop_children_asManyAsParents ::
  N -> NonEmpty Integer -> Property
prop_children_asManyAsParents nX is =
  again $
    monadicIO $
      do
        let e = IntTestEnviroment ((0,100000),10)
        mwc <- Test.QuickCheck.Monadic.run create
        is' <- Test.QuickCheck.Monadic.run $ sampleFrom mwc (children e nX is)
        return $ counterexample (show is') $ length is' == length is


prop_bestsBy_isBestsBy' :: Int -> Population Integer -> Property
prop_bestsBy_isBestsBy' k pop =
  k > 0 ==>
    monadicIO $
      do
        let a = fst $ bestsBy k (fitness NoData) pop
        let b = bestsBy' k (fitness NoData) pop
        assert $ NE.toList a == b

prop_bestsBy_lengths :: Int -> Population Integer -> Property
prop_bestsBy_lengths k pop =
  k > 0 ==> monadicIO $ do
    let (bests, rest) = bestsBy k (fitness NoData) pop
    assert $
      length bests == min k (length pop) && length bests + length rest == length pop
prop_stepSteady_constantPopSize ::
  NonEmpty Integer -> Property
prop_stepSteady_constantPopSize pop =
  forAll
    ( (,)
        <$> choose (1, length pop)
        <*> choose (1, length pop)
    )
    $ \(nParents, nX) -> monadicIO $ do
      let pElite = 0.1
      let eval = NoData
      let env = IntTestEnviroment ((0,100000),10)
      mwc <- Test.QuickCheck.Monadic.run create
      pop' <- Test.QuickCheck.Monadic.run $ sampleFrom mwc (stepSteady eval env (tournament eval 4) nParents nX pElite pop)
      return . counterexample (show pop') $ length pop' == length pop

prop_tournament_selectsN :: Int -> Int -> NonEmpty Integer -> Property
prop_tournament_selectsN nTrnmnt n pop =
  0 < nTrnmnt
    && nTrnmnt < length pop
    && 0 < n
    ==> monadicIO
    $ do
      mwc <- Test.QuickCheck.Monadic.run create
      pop' <- Test.QuickCheck.Monadic.run $ sampleFrom mwc (tournament NoData 2 n pop)
      assert $ length pop' == n

prop_withoutReplacement_selectsN :: Int -> NonEmpty a -> Property
prop_withoutReplacement_selectsN n pop =
  0 < n && n <= length pop ==> monadicIO (do
    mwc <- Test.QuickCheck.Monadic.run create
    pop' <- Test.QuickCheck.Monadic.run $ sampleFrom mwc (withoutReplacement n pop)
    assert $ length pop' == n)

prop_shuffle_length :: NonEmpty a -> Property
prop_shuffle_length xs = monadicIO(do
  mwc <- Test.QuickCheck.Monadic.run create
  xs' <- Test.QuickCheck.Monadic.run $ sampleFrom mwc (shuffle' xs)
  assert $ length xs' == length xs)

runTests :: IO Bool
runTests = $quickCheckAll

return []
