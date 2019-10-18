{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module GA where

-- NEXT commit everything
-- TODO add factory floor optimizer:
-- [2019-07-15] GA that optimizes factory floor
--   - data: graph of workstations with edge weights being the number of walks between them
--   - desired: optimal configuration that reduces crossings
--   - space: 15 workstations that can be positioned in a 20 x 20 space
import Control.Arrow hiding (first)
import qualified Data.List as L
import Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NE
import Data.Random
import Data.Random.Distribution.Categorical
import Data.Random.Sample
import Pretty
import Protolude
import Test.QuickCheck hiding (sample, shuffle)
import Test.QuickCheck.Instances
import Test.QuickCheck.Monadic

-- TODO Enforce this being > 0
type N = Int

type R = Double

-- alternative could be
-- data I a
--   = I
--       { mutate :: m (I a),
--         crossover1 :: (MonadRandom m) => I a -> m (Maybe (I a, I a))
--       }
class Eq i => Individual i where

  {-|
  Generates a completely random individual given an existing individual.
  
  We have to add @i@ here as a parameter in order to be able to inject stuff.
  
  TODO This (and also, Seminar.I, which contains an ugly parameter @p@) has to
  be done nicer!
  -}
  new :: (MonadRandom m) => i -> m i

  {-|
  Generates a random population of the given size.
  -}
  population :: (MonadRandom m) => N -> i -> m (Population i)
  population 0 _ = undefined
  population n i = Pop . NE.fromList <$> replicateM n (new i)

  mutate :: (MonadRandom m) => i -> m i

  crossover1 :: (MonadRandom m) => i -> i -> m (Maybe (i, i))

  -- TODO Perhaps rather add a 'features' function (and parametrize select1 etc. with fitness function)?
  fitness :: (Monad m) => i -> m R

  {-|
  Performs an n-point crossover.
  
  Given the function for single-point crossover, 'crossover1', this function can
  be derived through recursion and a monad combinator (which is also the default
  implementation).
  -}
  crossover :: (MonadRandom m) => Int -> i -> i -> m (Maybe (i, i))
  crossover n i1 i2
    | n <= 0 = return $ Just (i1, i2)
    | otherwise = do
      isM <- crossover1 i1 i2
      maybe (return Nothing) (uncurry (crossover (n - 1))) isM

-- TODO Perhaps use Data.Vector.Sized for the population?
{-|
It would be nice to model populations as GADTs but then no functor instance were
possible:
> data Population a where
>  Pop :: Individual a => NonEmpty a -> Population a
-}
newtype Population a = Pop {unPop :: NonEmpty a}
  deriving (Foldable, Functor, Semigroup, Show, Traversable)

instance (Arbitrary i) => Arbitrary (Population i) where
  arbitrary = Pop <$> arbitrary

{-|
Selects one individual from the population using proportionate selection.
-}
proportionate1 :: (Individual i, MonadRandom m) => Population i -> m i
proportionate1 pop =
  sequence ((\i -> (,i) <$> fitness i) <$> pop)
    >>= sample . fromWeightedList . NE.toList . unPop

-- TODO Perhaps use stochastic acceptance for performance?

{-|
Selects @n@ individuals from the population using proportionate selection.
-}
-- TODO Perhaps use Data.Vector.Sized for the result?
proportionate
  :: (Individual i, MonadRandom m)
  => N
  -> Population i
  -> m (NonEmpty i)
proportionate n pop
  | n > 1 = (<|) <$> proportionate1 pop <*> proportionate (n - 1) pop
  | otherwise = (:|) <$> proportionate1 pop <*> return []

{-|
Produce offspring circularly.
-}
children :: (Individual i, MonadRandom m) => N -> NonEmpty i -> m (NonEmpty i)
children _ (i :| []) = (:| []) <$> mutate i
children nX (i1 :| [i2]) = children2 nX i1 i2
children nX (i1 :| i2 : is') =
  (<>) <$> children2 nX i1 i2 <*> children nX (NE.fromList is')

children2 :: (Individual i, MonadRandom m) => N -> i -> i -> m (NonEmpty i)
children2 nX i1 i2 = do
  -- TODO Add crossover probability?
  (i3, i4) <- fromMaybe (i1, i2) <$> crossover nX i1 i2
  i5 <- mutate i3
  i6 <- mutate i4
  return $ i5 :| [i6]

{-|
The @k@ best individuals in the population when comparing using the supplied
function.
-}
bestBy :: (Individual i, Monad m) => N -> (i -> m R) -> Population i -> m [i]
bestBy k f =
  fmap (NE.take k . fmap fst . NE.sortBy (comparing (Down . snd)))
    . traverse (\i -> (i,) <$> f i)
    . unPop

-- TODO no trivial instance for worst
-- prop_worstLength :: Int -> Population Int -> Property
-- prop_worstLength k pop = monadicIO $ (k ==) . length <$> worst k pop
{-|
The @k@ worst individuals in the population.
-}
worst :: (Individual i, Monad m) => N -> Population i -> m [i]
worst = flip bestBy (fmap (1 /) . fitness)

{-|
The @k@ best individuals in the population.
-}
bests :: (Individual i, Monad m) => N -> Population i -> m [i]
bests = flip bestBy fitness

{-|
Runs the GA and prints the @nResult@ best individuals.
-}
ga' nParents nX pop term nResult = do
  pop <- ga nParents nX pop term
  res <- bests nResult pop
  sequence $ format <$> res
  where
    -- TODO this has to be done nicer
    format :: (Individual i, MonadIO m, Pretty i) => i -> m ()
    format s = do
      f <- liftIO $ fitness s
      putText $ show f <> "\n" <> pretty s

{-|
Runs the GA, using in each iteration
 - @nParents@ parents for creating @nParents@ children and
 - @nX@-point crossover.

It terminates after the termination criterion is fulfilled.
-}
ga
  :: (Individual i, MonadRandom m, Monad m)
  => N
  -> N
  -> Population i
  -> Termination i
  -> m (Population i)
ga nParents nX pop term = ga' nParents nX pop term 0
  where
    ga'
      :: (Individual i, MonadRandom m, Monad m)
      => N
      -> N
      -> Population i
      -> Termination i
      -> N
      -> m (Population i)
    ga' nParents nX pop term t = do
      -- trace (show t <> ": " <> show (length pop)) $ return ()
      is <- proportionate nParents pop
      i :| is' <- children nX is
      -- traceShow (length is') $ return ()
      iWorsts <- worst nParents pop
      -- traceShow (length iWorsts) $ return ()
      let popClean = foldr L.delete (NE.toList . unPop $ pop) iWorsts
      -- traceShow (length popClean) $ return ()
      -- for the fromList to not fail, n < length pop
      -- replace the worst ones
      let pop' = Pop $ i :| is' <> popClean
      -- replace fitness proportionally
      -- let pop' = Pop <$> proportionate (length pop) (pop <> Pop is')
      if term pop' t
        then return pop'
        else ga' nParents nX pop' term (t + 1)

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
