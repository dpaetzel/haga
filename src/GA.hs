{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : GA
Description : Abstract genetic algorithm
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Simplistic abstract definition of a genetic algorithm.

In order to use it for a certain problem, basically, you have to make your
solution type an instance of 'Individual' and then simply call the 'run'
function.
-}
module GA where

import Control.Arrow hiding (first, second)
import qualified Data.List as L
import Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NE
import qualified Data.List.NonEmpty.Extra as NE (appendl, sortOn)
import Data.Random
import Data.Random.Distribution.Categorical
import Data.Random.Sample
import Pipes
import Pretty
import Protolude
import Test.QuickCheck hiding (sample, shuffle)
import Test.QuickCheck.Instances
import Test.QuickCheck.Monadic

-- TODO there should be a few 'shuffle's here

-- TODO enforce this being > 0
type N = Int

type R = Double

class Eq i => Individual i where

  {-|
  Generates a completely random individual given an existing individual.
  
  We have to add @i@ here as a parameter in order to be able to inject stuff.
  -}
  -- TODO This (and also, Seminar.I, which contains an ugly parameter @p@) has
  -- to be done nicer!
  new :: (MonadRandom m) => i -> m i

  {-|
  Generates a random population of the given size.
  -}
  population :: (MonadRandom m) => N -> i -> m (Population i)
  population n i
    | n <= 0 = undefined
    | otherwise = NE.fromList <$> replicateM n (new i)

  mutate :: (MonadRandom m) => i -> m i

  crossover1 :: (MonadRandom m) => i -> i -> m (Maybe (i, i))

  {-|
  An individual's fitness. Higher values are considered “better”.
  
  We explicitely allow fitness values to be have any sign (see, for example,
  'proportionate1').
  -}
  fitness :: (Monad m) => i -> m R

  {-|
  Performs an n-point crossover.
  
  Given the function for single-point crossover, 'crossover1', this function can
  be derived through recursion and a monad combinator (which is also the default
  implementation).
  -}
  crossover :: (MonadRandom m) => N -> i -> i -> m (Maybe (i, i))
  crossover n i1 i2
    | n <= 0 = return $ Just (i1, i2)
    | otherwise = do
      isM <- crossover1 i1 i2
      maybe (return Nothing) (uncurry (crossover (n - 1))) isM

{-|
Needed for QuickCheck tests, for now, a very simplistic implementation should
suffice.
-}
instance Individual Integer where

  new _ = sample $ uniform 0 (0 + 100000)

  mutate i = sample $ uniform (i - 10) (i + 10)

  crossover1 i1 i2 = return $ Just (i1 - i2, i2 - i1)

  fitness = return . fromIntegral . negate

{-|
Populations are just basic non-empty lists.
-}
type Population i = NonEmpty i

{-|
Selects one individual from the population using proportionate selection.
-}
proportionate1 :: (Individual i, MonadRandom m) => Population i -> m i
proportionate1 pop =
  sequence ((\i -> (,i) <$> fitness i) <$> pop)
    >>= sample . fromWeightedList . NE.toList

{-|
Selects @n@ individuals from the population using proportionate selection.
-}
proportionate
  :: (Individual i, MonadRandom m)
  => N
  -> Population i
  -> m (NonEmpty i)
proportionate n pop
  | n > 1 = (<|) <$> proportionate1 pop <*> proportionate (n - 1) pop
  | otherwise = (:|) <$> proportionate1 pop <*> return []

{-|
Produces offspring circularly from the given list of parents.
-}
children
  :: (Individual i, MonadRandom m)
  => N -- ^ The @nX@ of the @nX@-point crossover operator
  -> NonEmpty i
  -> m (NonEmpty i)
children _ (i :| []) = (:| []) <$> mutate i
children nX (i1 :| [i2]) = children2 nX i1 i2
children nX (i1 :| i2 : is') =
  (<>) <$> children2 nX i1 i2 <*> children nX (NE.fromList is')

prop_children_asManyAsParents nX is =
  again
    $ monadicIO
    $ do
      is' <- lift $ children nX is
      return $ counterexample (show is') $ length is' == length is

children2 :: (Individual i, MonadRandom m) => N -> i -> i -> m (NonEmpty i)
children2 nX i1 i2 = do
  -- TODO Add crossover probability?
  (i3, i4) <- fromMaybe (i1, i2) <$> crossover nX i1 i2
  i5 <- mutate i3
  i6 <- mutate i4
  return $ i5 :| [i6]

{-|
The best according to a function, return up to @k@ results and the remaining
population.

If @k <= 0@, this returns the best one anyway (as if @k == 1@).
-}
bestsBy
  :: (Individual i, Monad m)
  => N
  -> (i -> m R)
  -> Population i
  -> m (NonEmpty i, [i])
bestsBy k f pop@(i :| pop')
  | k <= 0 = bestsBy 1 f pop
  | otherwise = foldM run (i :| [], []) pop'
  where
    run (bests, rest) i =
      ((NE.fromList . NE.take k) &&& (rest <>) . NE.drop k)
        <$> sorted (i <| bests)
    sorted =
      fmap (fmap fst . NE.sortOn (Down . snd)) . traverse (\i -> (i,) <$> f i)

{-|
The @k@ best individuals in the population when comparing using the supplied
function.
-}
bestsBy' :: (Individual i, Monad m) => N -> (i -> m R) -> Population i -> m [i]
bestsBy' k f =
  fmap (NE.take k . fmap fst . NE.sortBy (comparing (Down . snd)))
    . traverse (\i -> (i,) <$> f i)

prop_bestsBy_isBestsBy' k pop =
  k > 0
    ==> monadicIO
    $ do
      a <- fst <$> bestsBy k fitness pop
      b <- bestsBy' k fitness pop
      assert $ NE.toList a == b

prop_bestsBy_lengths k pop =
  k > 0 ==> monadicIO $ do
    (bests, rest) <- bestsBy k fitness pop
    assert
      $ length bests == min k (length pop) && length bests + length rest == length pop

{-|
The @k@ worst individuals in the population.
-}
worst :: (Individual i, Monad m) => N -> Population i -> m (NonEmpty i, [i])
worst = flip bestsBy (fmap negate . fitness)

{-|
The @k@ best individuals in the population.
-}
bests :: (Individual i, Monad m) => N -> Population i -> m (NonEmpty i, [i])
bests = flip bestsBy fitness

-- TODO add top x percent parent selection (select n guys, sort by fitness first)
{-|
Performs one iteration of the genetic algorithm.
-}
step
  :: (Individual i, MonadRandom m, Monad m)
  => N -- ^ number of parents @nParents@ for creating @nParents@ children
  -> N -- ^ how many crossover points (the @nX@ in @nX@-point crossover)
  -> R -- ^ elitism ratio @pElite@
  -> Population i
  -> m (Population i)
-- TODO parametrize selection: 'proportionate' and 'worst'
step nParents nX pElite pop = do
  iParents <- proportionate nParents pop
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
            . fst <$> bests (length pop - length elitists) (i :| is)
  where
    nBest = floor . (pElite *) . fromIntegral $ NE.length pop

prop_stepSteady_constantPopSize pop =
  forAll
    ( (,)
        <$> choose (1, length pop)
        <*> choose (1, length pop)
    )
    $ \(nParents, nX) -> monadicIO $ do
      let pElite = 0.1
      pop' <- lift $ stepSteady (tournament 4) nParents nX pElite pop
      return . counterexample (show pop') $ length pop' == length pop

{-|
Given an initial population, runs the GA until the termination criterion is
fulfilled.

Uses the pipes library to, in each step, 'Pipes.yield' the currently best known
solution.
-}
run
  :: (Individual i, Monad m, MonadRandom m)
  => N -- ^ number of parents @nParents@ for creating @nParents@ children
  -> N -- ^ how many crossover points (the @nX@ in @nX@-point crossover)
  -> R -- ^ elitism ratio @pElite@
  -> Population i
  -> Termination i
  -> Producer (Int, R) m (Population i)
run nParents nX pElite pop term = step' 0 pop
  where
    step' t pop
      | term pop t = return pop
      | otherwise = do
        pop' <- lift $ step nParents nX pElite pop
        (iBests, _) <- lift $ bests 1 pop'
        fs <- lift . sequence $ fitness <$> iBests
        let fBest = NE.head fs
        yield (t, fBest)
        step' (t + 1) pop'

-- * Termination criteria

{-|
Termination decisions may take into account the current population and the
current iteration number.
-}
type Termination i = Population i -> N -> Bool

{-|
Termination after a number of steps.
-}
steps :: N -> Termination i
steps tEnd _ t = t >= tEnd

-- * Helper functions

{-|
Shuffles a non-empty list.
-}
shuffle' :: (MonadRandom m) => NonEmpty a -> m (NonEmpty a)
shuffle' xs@(x :| []) = return xs
shuffle' xs = do
  i <- sample . uniform 0 $ NE.length xs - 1
  -- slightly unsafe (!!) used here so deletion is faster
  let x = xs NE.!! i
  xs' <- sample . shuffle $ deleteI i xs
  return $ x :| xs'
  where
    deleteI i xs = fst (NE.splitAt i xs) ++ snd (NE.splitAt (i + 1) xs)

prop_shuffle_length xs = monadicIO $ do
  xs' <- lift $ shuffle' xs
  assert $ length xs' == length xs

return []

runTests = $quickCheckAll
