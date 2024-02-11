{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
module GA where

import Control.Arrow hiding (first, second)
import Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NE
import qualified Data.List.NonEmpty.Extra as NE (appendl, sortOn)
import Data.Random
import System.Random.MWC (create)
import Pipes
import Protolude
import Test.QuickCheck hiding (sample, shuffle)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic

-- TODO there should be a few 'shuffle's here

-- TODO enforce this being > 0
type N = Int

type R = Double

class Eq i => Individual i where
  -- |
  --  Generates a completely random individual given an existing individual.
  --
  --  We have to add @i@ here as a parameter in order to be able to inject stuff.

  -- TODO This (and also, Seminar.I, which contains an ugly parameter @p@) has
  -- to be done nicer!
  new :: i -> RVar i

  -- |
  --  Generates a random population of the given size.
  population :: N -> i -> RVar (Population i)
  population n i
    | n <= 0 = undefined
    | otherwise = NE.fromList <$> replicateM n (new i)

  mutate :: i -> RVar i

  crossover1 :: i -> i -> RVar (Maybe (i, i))

  -- |
  --  An individual's fitness. Higher values are considered “better”.
  --
  --  We explicitely allow fitness values to be have any sign (see, for example,
  --  'proportionate1').
  fitness :: (Monad m) => i -> m R

  -- |
  --  Performs an n-point crossover.
  --
  --  Given the function for single-point crossover, 'crossover1', this function can
  --  be derived through recursion and a monad combinator (which is also the default
  --  implementation).
  crossover :: N -> i -> i -> RVar (Maybe (i, i))
  crossover n i1 i2
    | n <= 0 = return $ Just (i1, i2)
    | otherwise = do
        isM <- crossover1 i1 i2
        maybe (return Nothing) (uncurry (crossover (n - 1))) isM

-- |
-- Needed for QuickCheck tests, for now, a very simplistic implementation should
-- suffice.
instance Individual Integer where
  new _ = uniform 0 (0 + 100000)

  mutate i = uniform (i - 10) (i + 10)

  crossover1 i1 i2 = return $ Just (i1 - i2, i2 - i1)

  fitness = return . fromIntegral . negate

-- |
-- Populations are just basic non-empty lists.
type Population i = NonEmpty i

-- |
-- Produces offspring circularly from the given list of parents.
children ::
  (Individual i) =>
  -- | The @nX@ of the @nX@-point crossover operator
  N ->
  NonEmpty i ->
  RVar (NonEmpty i)
children _ (i :| []) = (:| []) <$> mutate i
children nX (i1 :| [i2]) = children2 nX i1 i2
children nX (i1 :| i2 : is') =
  (<>) <$> children2 nX i1 i2 <*> children nX (NE.fromList is')

prop_children_asManyAsParents ::
  (Individual a, Show a) => N -> NonEmpty a -> Property
prop_children_asManyAsParents nX is =
  again $
    monadicIO $
      do
        mwc <- Test.QuickCheck.Monadic.run create
        is' <- Test.QuickCheck.Monadic.run $ sampleFrom mwc (children nX is)
        return $ counterexample (show is') $ length is' == length is

children2 :: (Individual i) => N -> i -> i -> RVar (NonEmpty i)
children2 nX i1 i2 = do
  -- TODO Add crossover probability?
  (i3, i4) <- fromMaybe (i1, i2) <$> crossover nX i1 i2
  i5 <- mutate i3
  i6 <- mutate i4
  return $ i5 :| [i6]

-- |
-- The best according to a function; returns up to @k@ results and the remaining
-- population.
--
-- If @k <= 0@, this returns the best one anyway (as if @k == 1@).
bestsBy ::
  (Individual i, Monad m) =>
  N ->
  (i -> m R) ->
  Population i ->
  m (NonEmpty i, [i])
bestsBy k f pop@(i :| pop')
  | k <= 0 = bestsBy 1 f pop
  | otherwise = foldM run (i :| [], []) pop'
  where
    run (bests, rest) i =
      ((NE.fromList . NE.take k) &&& (rest <>) . NE.drop k)
        <$> sorted (i <| bests)
    sorted =
      fmap (fmap fst . NE.sortOn (Down . snd)) . traverse (\i -> (i,) <$> f i)

-- |
-- The @k@ best individuals in the population when comparing using the supplied
-- function.
bestsBy' :: (Individual i, Monad m) => N -> (i -> m R) -> Population i -> m [i]
bestsBy' k f =
  fmap (NE.take k . fmap fst . NE.sortBy (comparing (Down . snd)))
    . traverse (\i -> (i,) <$> f i)

prop_bestsBy_isBestsBy' :: Individual a => Int -> Population a -> Property
prop_bestsBy_isBestsBy' k pop =
  k > 0 ==>
    monadicIO $
      do
        a <- fst <$> bestsBy k fitness pop
        b <- bestsBy' k fitness pop
        assert $ NE.toList a == b

prop_bestsBy_lengths :: Individual a => Int -> Population a -> Property
prop_bestsBy_lengths k pop =
  k > 0 ==> monadicIO $ do
    (bests, rest) <- bestsBy k fitness pop
    assert $
      length bests == min k (length pop) && length bests + length rest == length pop

-- |
-- The @k@ worst individuals in the population (and the rest of the population).
worst :: (Individual i, Monad m) => N -> Population i -> m (NonEmpty i, [i])
worst = flip bestsBy (fmap negate . fitness)

-- |
-- The @k@ best individuals in the population (and the rest of the population).
bests :: (Individual i, Monad m) => N -> Population i -> m (NonEmpty i, [i])
bests = flip bestsBy fitness

-- TODO add top x percent parent selection (select n guys, sort by fitness first)

-- |
-- Performs one iteration of a steady state genetic algorithm that in each
-- iteration that creates @k@ offspring simply deletes the worst @k@ individuals
-- while making sure that the given percentage of elitists survive (at least 1
-- elitist, even if the percentage is 0 or low enough for rounding to result in 0
-- elitists).
stepSteady ::
  (Individual i) =>
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
stepSteady select nParents nX pElite pop = do
  -- TODO Consider keeping the fitness evaluations already done for pop (so we
  -- only reevaluate iChildren)
  iParents <- select nParents pop
  iChildren <- NE.filter (`notElem` pop) <$> children nX iParents
  let pop' = pop `NE.appendl` iChildren
  (elitists, rest) <- bests nBest pop'
  case rest of
    [] -> return elitists
    (i : is) ->
      -- NOTE 'bests' always returns at least one individual, thus we need this
      -- slightly ugly branching
      if length elitists == length pop
        then return elitists
        else
          (elitists <>)
            . fst
            <$> bests (length pop - length elitists) (i :| is)
  where
    nBest = floor . (pElite *) . fromIntegral $ NE.length pop

prop_stepSteady_constantPopSize ::
  (Individual a, Show a) => NonEmpty a -> Property
prop_stepSteady_constantPopSize pop =
  forAll
    ( (,)
        <$> choose (1, length pop)
        <*> choose (1, length pop)
    )
    $ \(nParents, nX) -> monadicIO $ do
      let pElite = 0.1
      mwc <- Test.QuickCheck.Monadic.run create
      pop' <- Test.QuickCheck.Monadic.run $ sampleFrom mwc (stepSteady (tournament 4) nParents nX pElite pop)
      return . counterexample (show pop') $ length pop' == length pop

-- |
-- Given an initial population, runs the GA until the termination criterion is
-- fulfilled.
--
-- Uses the pipes library to, in each step, 'Pipes.yield' the currently best known
-- solution.
run ::
  (Individual i) =>
  -- | Mechanism for selecting parents
  Selection RVar i ->
  -- | Number of parents @nParents@ for creating @nParents@ children
  N ->
  -- | How many crossover points (the @nX@ in @nX@-point crossover)
  N ->
  -- | Elitism ratio @pElite@
  R ->
  RVar (Population i) ->
  Termination i ->
  Producer (Int, R) IO (Population i)
run select nParents nX pElite pop term = do
        mwc <- lift create
        let x = \currPop generation -> do
              currPop' <- lift $ sampleFrom mwc $ currPop
              if term currPop' generation
                then return currPop'
                else do
                  let nextPop = stepSteady select nParents nX pElite currPop'
                  nextPop' <- lift $ sampleFrom mwc $ nextPop
                  (iBests, _) <- lift $ bests 1 nextPop'
                  fs <- lift . sequence $ fitness <$> iBests
                  let fBest = NE.head fs
                  Pipes.yield (generation, fBest)
                  x nextPop (generation + 1)
        x pop 0


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
tournament :: (Individual i) => N -> Selection RVar i
tournament nTrnmnt = chain (tournament1 nTrnmnt)

prop_tournament_selectsN :: Individual a => Int -> Int -> NonEmpty a -> Property
prop_tournament_selectsN nTrnmnt n pop =
  0 < nTrnmnt
    && nTrnmnt < length pop
    && 0 < n
    ==> monadicIO
    $ do
      mwc <- Test.QuickCheck.Monadic.run create
      pop' <- Test.QuickCheck.Monadic.run $ sampleFrom mwc (tournament 2 n pop)
      assert $ length pop' == n

-- |
-- Selects one individual from the population using tournament selection.
tournament1 ::
  (Individual i) =>
  -- | Tournament size
  N ->
  Population i ->
  RVar i
tournament1 nTrnmnt pop
  -- TODO Use Positive for this constraint
  | nTrnmnt <= 0 = undefined
  | otherwise = trnmnt >>= fmap (NE.head . fst) . bests 1
  where
    trnmnt = withoutReplacement nTrnmnt pop

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

prop_withoutReplacement_selectsN :: Int -> NonEmpty a -> Property
prop_withoutReplacement_selectsN n pop =
  0 < n && n <= length pop ==> monadicIO (do
    mwc <- Test.QuickCheck.Monadic.run create
    pop' <- Test.QuickCheck.Monadic.run $ sampleFrom mwc (withoutReplacement n pop)
    assert $ length pop' == n)

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

prop_shuffle_length :: NonEmpty a -> Property
prop_shuffle_length xs = monadicIO(do
  mwc <- Test.QuickCheck.Monadic.run create
  xs' <- Test.QuickCheck.Monadic.run $ sampleFrom mwc (shuffle' xs)
  assert $ length xs' == length xs)

return []

runTests :: IO Bool
runTests = $quickCheckAll
