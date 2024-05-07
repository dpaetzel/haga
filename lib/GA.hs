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
module GA (Environment (..), Fitness (..), Evaluator (..), Individual, GA.run, Tournament (..), N, R, Population, steps, bests, runTests, GaRunConfig (..)) where

import Control.Arrow hiding (first, second)
import Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NE
import qualified Data.List.NonEmpty.Extra as NE (appendl)
import qualified Data.Map.Strict as Map
import Data.Random
import Pipes
import Pretty
import Protolude
import Protolude.Error
import System.Random.MWC (create, createSystemRandom)
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
class (Individual i) => Environment i e | e -> i, i -> e where
  output :: e -> i -> Text

  -- |
  --  Generates a completely random individual.
  new :: e -> RVar i

  -- |
  --  Generates a random population of the given size.
  population :: e -> N -> RVar (Population i)
  population env n
    | n <= 0 = error "nonPositive in population"
    | otherwise = NE.fromList <$> replicateM n (new env)

  mutate :: e -> i -> RVar i

  crossover1 :: e -> i -> i -> RVar (Maybe (i, i))

  nX :: e -> N

  -- |
  --  Performs an n-point crossover.
  --
  --  Given the function for single-point crossover, 'crossover1', this function can
  --  be derived through recursion and a monad combinator (which is also the default
  --  implementation).
  crossover :: e -> i -> i -> RVar (Maybe (i, i))
  crossover e = crossover' e (nX e)

  crossover' :: e -> N -> i -> i -> RVar (Maybe (i, i))
  crossover' env n i1 i2
    | n <= 0 = return $ Just (i1, i2)
    | otherwise = do
        isM <- crossover1 env i1 i2
        maybe (return Nothing) (uncurry (crossover' env (n - 1))) isM

-- |
--  An Evaluator that Individuals of type i can be evaluated by
--  It stores all information required to evaluate an individuals fitness
class (Individual i, Fitness r) => Evaluator i e r | e -> i r where
  -- |
  --  An individual's fitness. Higher values are considered “better”.
  --
  --  We explicitely allow fitness values to be have any sign (see, for example,
  --  'proportionate1').
  fitness :: e -> i -> R
  fitness env i = getR ( fitness' env i)

  -- |
  --  An more complete fitness object, used to include more info to the output of the current fitness.
  --  You can e.g. track individual size with this.
  fitness' :: e -> i -> r

  -- |
  -- here, fitness values for the next generation can be calculated at once, and just once, using any monadic action, if necessary.
  -- It is guaranteed that the e passed to fitness is the result of a calc function, where the individual was part of the Population passed.
  -- It may be smart to reuse known results between invocations.
  calc :: e -> Population i -> IO e
  calc eval _ = return eval

class (Ord i) => Individual i

class (Show i) => Fitness i where
  getR :: i -> R

instance Fitness Double where
  getR d = d

-- |
-- Populations are just basic non-empty lists.
type Population i = NonEmpty i

-- |
-- Produces offspring circularly from the given list of parents.
children ::
  (Individual i, Environment i e) =>
  e ->
  NonEmpty i ->
  RVar (NonEmpty i)
children e (i :| []) = (:| []) <$> mutate e i
children e (i1 :| [i2]) = children2 e i1 i2
children e (i1 :| i2 : is') =
  (<>) <$> children2 e i1 i2 <*> children e (NE.fromList is')

children2 :: (Individual i, Environment i e) => e -> i -> i -> RVar (NonEmpty i)
children2 e i1 i2 = do
  -- TODO Add crossover probability?
  (i3, i4) <- fromMaybe (i1, i2) <$> crossover e i1 i2
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
  | otherwise =
      let (elites, rest) = NE.splitAt k $ map fst $ NE.sortBy (comparing (Down . snd)) $ map (\i -> (i, f i)) pop
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
worst :: (Individual i, Evaluator i e r) => e -> N -> Population i -> (NonEmpty i, [i])
worst e k = bestsBy k (negate . fitness e)

-- |
-- The @k@ best individuals in the population (and the rest of the population).
bests :: (Individual i, Evaluator i e r) => e -> N -> Population i -> (NonEmpty i, [i])
bests e k = bestsBy k (fitness e)

-- TODO add top x percent parent selection (select n guys, sort by fitness first)

reproduce ::
  (Individual i, Environment i env, Evaluator i eval r, SelectionType s) =>
  eval ->
  env ->
  -- | Mechanism for selecting parents
  s ->
  -- | Number of parents @nParents@ for creating @nParents@ children
  N ->
  Population i ->
  RVar (Population i)
reproduce eval env selectT nParents pop = do
  iParents <-select selectT nParents pop eval
  iChildren <- NE.filter (`notElem` pop) <$> children env iParents
  let pop' = pop `NE.appendl` iChildren
  return pop'

selectBest ::
  (Individual i, Evaluator i eval r) =>
  eval ->
  -- | Elitism ratio @pElite@
  R ->
  Population i ->
  -- | How many individuals should be selected
  N ->
  RVar (Population i)
selectBest eval pElite pop nPop = do
  let eliteSize = floor . (pElite *) . fromIntegral $ nPop
  let (elitists, rest) = bests eval eliteSize pop
  case rest of
    [] -> return elitists
    _notEmpty ->
      -- NOTE 'bests' always returns at least one individual, thus we need this
      -- slightly ugly branching
      if length elitists == nPop
        then return elitists
        else return $ elitists <> (fst $ bests eval (nPop - length elitists) (NE.fromList rest))


-- This class encapsulates everything needed to run a generic genetic Algorithm
data GaRunConfig i r eval env t where
  GaRunConfig :: (Individual i, Fitness r, Evaluator i eval r, Environment i env, SelectionType t) => {
    enviroment :: env,
    initialEvaluator :: eval,
    selectionType :: t,
    termination :: (Termination i),
    poulationSize :: N,
    stepSize :: N,
    elitismRatio :: R
  } -> GaRunConfig i r eval env t


run :: GaRunConfig i r eval env t -> Producer (Int, r) IO (Population i)
run config@(GaRunConfig _ _ _ _ _ _ _) = do
  let eval = initialEvaluator config
  let env = enviroment config
  let nPop = poulationSize config
  mwc <- liftIO createSystemRandom
  let smpl = ((sampleFrom mwc) :: RVar a -> IO a)
  firstPop <- liftIO $ smpl $ (population env nPop)
  res <- runIter eval 0 firstPop smpl
  return res
  where
    runIter eval count pop smpl = (
      if (termination config) pop count
        then do
          return pop
        else do
          let env = enviroment config
          let nPop = poulationSize config
          let selecType = selectionType config
          let nParents = stepSize config
          let pElite = elitismRatio config
          eval <- liftIO $ calc eval pop
          withKids <- liftIO $ smpl $ reproduce eval env selecType nParents pop
          eval <- liftIO $ calc eval withKids
          resPop <- liftIO $ smpl $ selectBest eval pElite withKids nPop
          let fBest = fitness' eval $ NE.head $ fst $ bests eval 1 resPop
          Pipes.yield (count, fBest)
          res <- runIter eval (count + 1) resPop smpl
          return res)

-- * Selection mechanisms

-- |
-- A function generating a monadic action which selects a given number of
-- individuals from the given population.
data Tournament = Tournament N

class SelectionType t where
  select :: (Individual i, Evaluator i e r) => t -> N -> Population i -> e -> RVar (NonEmpty i)

-- type Selection m i = N -> Population i -> m (NonEmpty i)

instance SelectionType Tournament where
  select (Tournament i) count pop eval = fmap NE.fromList (replicateM count (tournament1 eval i pop))

-- |
-- Selects one individual from the population using tournament selection.
tournament1 ::
  (Individual i, Evaluator i e r) =>
  e ->
  -- | Tournament size
  N ->
  Population i ->
  RVar i
tournament1 eval nTrnmnt pop
  -- TODO Use Positive for this constraint
  | nTrnmnt <= 0 = error "nonPositive in tournament1"
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
withoutReplacement 0 _ = error "0 in withoutReplacement"
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


instance Individual Integer

newtype IntTestEnviroment = IntTestEnviroment ((Integer, Integer), Integer, N) deriving (Eq, Show) -- IntTestEnviroment ((0,100000),10)

instance Pretty IntTestEnviroment where
  -- instance Pretty (Maybe Student) where
  pretty (IntTestEnviroment ((i, j), k, _)) = "IntTestEnviroment of Individuals between " <> (show i) <> " and " <> (show j) <> " variance when mutating is " <> (show k)

instance Environment Integer IntTestEnviroment where
  output _ i = "Found int: " <> show i

  new (IntTestEnviroment ((from, to), _, _)) = uniform from to

  nX (IntTestEnviroment ((_, _), _, n)) = n

  mutate (IntTestEnviroment ((from, to), wiggle, _)) i = uniform (max from (i - wiggle)) (min to (i + wiggle))

  crossover1 _ i1 i2 = do
    i1' <- uniform i1 i2
    i2' <- uniform i1 i2
    return $ Just (i1', i2')

data NoData = NoData deriving (Eq)

instance Evaluator Integer NoData Double where
  fitness _ = fromIntegral . negate

prop_children_asManyAsParents ::
  N -> NonEmpty Integer -> Property
prop_children_asManyAsParents nX is =
  again $
    monadicIO $
      do
        let e = IntTestEnviroment ((0, 100000), 10, nX)
        mwc <- Test.QuickCheck.Monadic.run create
        is' <- Test.QuickCheck.Monadic.run $ sampleFrom mwc (children e is)
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

-- TODO: re-add!
-- prop_stepSteady_constantPopSize ::
--  NonEmpty Integer -> Property
-- prop_stepSteady_constantPopSize pop =
--  forAll
--    ( (,)
--        <$> choose (1, length pop)
--        <*> choose (1, length pop)
--    )
--    $ \(nParents, nX) -> monadicIO $ do
--      let pElite = 0.1
--      let eval = NoData
--      let env = IntTestEnviroment ((0, 100000), 10, nX)
--      mwc <- Test.QuickCheck.Monadic.run create
--      pop' <- Test.QuickCheck.Monadic.run $ sampleFrom mwc (stepSteady eval env (tournament eval 4) nParents nX pElite pop)
--      return . counterexample (show pop') $ length pop' == length pop

prop_tournament_selectsN :: Int -> Int -> NonEmpty Integer -> Property
prop_tournament_selectsN nTrnmnt n pop =
  0 < nTrnmnt
    && nTrnmnt < length pop
    && 0 < n
    ==> monadicIO
    $ do
      mwc <- Test.QuickCheck.Monadic.run create
      pop' <- Test.QuickCheck.Monadic.run $ sampleFrom mwc (select (Tournament 2) n pop NoData)
      assert $ length pop' == n

prop_withoutReplacement_selectsN :: Int -> NonEmpty a -> Property
prop_withoutReplacement_selectsN n pop =
  0 < n && n <= length pop ==>
    monadicIO
      ( do
          mwc <- Test.QuickCheck.Monadic.run create
          pop' <- Test.QuickCheck.Monadic.run $ sampleFrom mwc (withoutReplacement n pop)
          assert $ length pop' == n
      )

prop_shuffle_length :: NonEmpty a -> Property
prop_shuffle_length xs =
  monadicIO
    ( do
        mwc <- Test.QuickCheck.Monadic.run create
        xs' <- Test.QuickCheck.Monadic.run $ sampleFrom mwc (shuffle' xs)
        assert $ length xs' == length xs
    )

runTests :: IO Bool
runTests = $quickCheckAll

return []
