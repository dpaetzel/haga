{-# LANGUAGE NoImplicitPrelude #-}

module Utils where

import GA (R)
import Protolude

takeFraktion :: (RealFrac f) => f -> [a] -> [a]
takeFraktion frac list = take (floor (frac * (fromIntegral (length list)))) list

dropFraktion :: (RealFrac f) => f -> [a] -> [a]
dropFraktion frac list = drop (floor (frac * (fromIntegral (length list)))) list

meanOfAccuricyPerClass :: (Enum r, Bounded r, Eq r) => [(r, r)] -> R
meanOfAccuricyPerClass results = mean $ map (accuracyInClass results) [minBound .. maxBound]

geomeanOfAccuricyPerClass :: (Enum r, Bounded r, Eq r) => [(r, r)] -> R
geomeanOfAccuricyPerClass results = geomean $ map (accuracyInClass results) [minBound .. maxBound]

geomeanOfDistributionAccuracy :: (Enum r, Bounded r, Eq r) => [(r, r)] -> R
geomeanOfDistributionAccuracy results = geomean $ map (distributionAccuracyForClass results) [minBound .. maxBound]

distributionAccuracyForClass :: (Eq r) => [(r, r)] -> r -> R
distributionAccuracyForClass results clas = (1 - (min 1 (fromIntegral (abs ((length (inResClass results clas)) - (length (inClass results clas)))) / fromIntegral (length (inClass results clas))))) * 100

mean :: (Show f, RealFloat f) => [f] -> f
mean values = (sum filteredValues) * (1 / (fromIntegral (length filteredValues)))
  where
    filteredValues = filter (not . isNaN) values

geomean :: (Show f, RealFloat f) => [f] -> f
geomean values = (product filteredValues) ** (1 / (fromIntegral (length filteredValues)))
  where
    filteredValues = filter (not . isNaN) values

accuracyInClass :: (Eq r) => [(r, r)] -> r -> R
accuracyInClass results clas = ((accuracy' (inResClass results clas)) * 100) / fromIntegral (length (inClass results clas))

inClass :: (Eq r) => [(r, r)] -> r -> [(r, r)]
inClass results clas = (filter ((clas ==) . fst) results)

inResClass :: (Eq r) => [(r, r)] -> r -> [(r, r)]
inResClass results clas = (filter ((clas ==) . snd) results)

accuracy' :: (Eq r) => [(r, r)] -> R
accuracy' results = fromIntegral $ length (filter (\(target, res) -> (res == target)) results)

repeatedly :: (a -> Maybe a) -> a -> [a]
repeatedly f x = case f x of
  Nothing -> []
  Just y -> y : repeatedly f y

contains :: (Eq a, Foldable t) => t a -> a -> Bool
contains list val = any (== val) list

count :: (Eq a) => [a] -> a -> Int
count [] _ = 0
count ys find = length xs
  where
    xs = [xs | xs <- ys, xs == find]
