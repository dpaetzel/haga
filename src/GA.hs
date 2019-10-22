{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module GA where

-- MAYBE add factory floor optimizer:
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
import Pipes
import Pretty
import Protolude
import Test.QuickCheck hiding (sample, shuffle)
import Test.QuickCheck.Instances

-- TODO using sample here was a quick hack
{-|
Shuffles a non-empty list.
-}
shuffle' :: (MonadRandom m) => NonEmpty a -> m (NonEmpty a)
shuffle' xs = do
  i <- sample . uniform 0 $ NE.length xs - 1
  let x = xs NE.!! i
  xs' <- sample . shuffle $ deleteI i xs
  return $ x :| xs'
  where
    deleteI i xs = fst (NE.splitAt (i - 1) xs) ++ snd (NE.splitAt i xs)

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

-- TODO there should be some shuffle here
{-|
The @k@ best individuals in the population when comparing using the supplied
function.
-}
bestsBy :: (Individual i, Monad m) => N -> (i -> m R) -> Population i -> m [i]
bestsBy k f =
  fmap (NE.take k . fmap fst . NE.sortBy (comparing (Down . snd)))
    . traverse (\i -> (i,) <$> f i)
    . unPop

{-|
The @k@ worst individuals in the population.
-}
worst :: (Individual i, Monad m) => N -> Population i -> m [i]
worst = flip bestsBy (fmap negate . fitness)

{-|
The @k@ best individuals in the population.
-}
bests :: (Individual i, Monad m) => N -> Population i -> m [i]
bests = flip bestsBy fitness

{-|
Runs the GA and prints the @nResult@ best individuals.
-}
ga' nParents nX pop term nResult = do
  pop <- run nParents nX pop term
  res <- bests nResult pop
  sequence $ format <$> res
  where
    -- TODO this has to be done nicer
    format :: (Individual i, MonadIO m, Pretty i) => i -> m ()
    format s = do
      f <- liftIO $ fitness s
      putText $ show f <> "\n" <> pretty s

-- TODO add top x percent selection (select n guys, sort by fitness first)

step
  :: (Individual i, MonadRandom m, Monad m)
  => N
  -> N
  -> Population i
  -> m (Population i)
step nParents nX pop = do
  iBests <- bests 1 pop
  is <- proportionate nParents pop
  i :| is' <- children nX is
  iWorsts <- worst nParents pop
  let popClean = foldr L.delete (NE.toList . unPop $ pop) $ iBests <> iWorsts
  -- TODO why does this not work? (we should use it!)
  -- Pop <$> (shuffle' . NE.nub $ i :| is' <> popClean <> iBests)
  return . Pop . NE.nub $ i :| is' <> popClean <> iBests

{-|
Runs the GA, using in each iteration
 - @nParents@ parents for creating @nParents@ children and
 - @nX@-point crossover.

It terminates after the termination criterion is fulfilled.
-}
run
  :: (Individual i, Monad m, MonadRandom m)
  => N
  -> N
  -> Population i
  -> Termination i
  -> Producer (Int, Maybe R) m (Population i)
run nParents nX pop term = step' 0 pop
  where
    step' t pop
      | term pop t = return pop
      | otherwise = do
        pop' <- lift $ step nParents nX pop
        iBests <- lift $ bests 1 pop'
        case headMay iBests of
          Just iBest -> do
            f <- fitness iBest
            yield (t, Just f)
          Nothing ->
            yield (t, Nothing)
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
