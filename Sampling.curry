{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module Sampling where

import Float ((^.), exp)
import List (groupBy, sortBy)
import Random (nextInt, getRandomSeed)
import SetFunctions (isEmpty, select, set0, values2list)

import Distributions (scale,scale')
import PFLP (Dist(..), Probability(..), value)

---- -------------------------------------------------------
----  Sampling
---- -------------------------------------------------------

filterByProb d@(Dist _ (Prob p)) rFloat | rFloat < p = d

sample :: Show a => Int -> Dist a -> IO a
sample = sample' False

sampleWithDebug :: Show a => Int -> Dist a -> IO a
sampleWithDebug = sample' True

sample' :: Show a => Bool -> Int -> Dist a -> IO a
sample' withDebug seed dA = do
  ds <- values2list (set0 dA)
  probSeed <- randomFloat seed
  let scaled   = scale' (map distToPair ds)
      split    = reverse (foldr splitProbAreas [] scaled)
      filtered = dropWhile ((probSeed >) . snd) split
  if withDebug
    then do
      putStrLn ("\nSeed: " ++ show seed)
      putStrLn ("\nPossible values: " ++ show scaled)
      putStrLn ("\nPossible values after split: " ++ show split)
      putStrLn ("\nPossible values after filter: " ++ show filtered)
    else return ()
  return (fst (head filtered))
 where
  splitProbAreas (val,prob) [] = [(val,prob)]
  splitProbAreas (val,prob)
                 vs@((_,prevProb):_)
                  = (val,prob + prevProb):vs

samples :: Show a => Int -> Int -> Dist a -> IO [a]
samples seed count dA = do
  let seeds = take count (nextInt seed)
  mapIO (\s -> sample s dA) seeds

distToPair :: Dist a -> (a,Float)
distToPair (Dist v (Prob p)) = (v,p)

randomFloat :: Int -> IO Float
randomFloat seed = do
  let val = head (nextInt seed)
      floatVal = fromInteger (abs val)
  return (truncate floatVal)
 where
  truncate val
    | val < 1.0 = val
    | otherwise = truncate (val / 5.0)

countSamples :: (Ord a, Show a) => Dist a -> IO [(a,Float)]
countSamples dA = do
  let times   = 10000
      prepare = map (\(val,count) -> (val, fromInteger count / fromInteger times)) . countBy (==) (<=)
  seed <- getRandomSeed
  values <- samples seed times dA
  return (prepare values)

countBy :: (a -> a -> Bool) -> (a -> a -> Bool) -> [a] -> [(a,Int)]
countBy pGroup pSort = map (\xs -> (head xs, length xs)) . groupBy p . sortBy pSort

---- -------------------------------------------------------
----  Hoeffd
---- -------------------------------------------------------

hoeffd :: Float -> Float -> Float -> Float -> Float
hoeffd n a b eps =
  2 * exp (- x / y)
 where
  x = 2 * n * (eps ^. 2)
  y = ((b - a) ^. 2)
