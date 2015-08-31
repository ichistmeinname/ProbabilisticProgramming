{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module Sampling where

import Float ((^.), exp)
import Random (nextIntRange, nextInt, getRandomSeed)
import SetFunctions (isEmpty, select, set0, values2list)

import Distributions (scale)
import PFLP (Dist(..), Probability(..), value)

---- -------------------------------------------------------
----  Sampling
---- -------------------------------------------------------

filterByProb d@(Dist _ (Prob p)) rFloat | rFloat < p = d

sample :: Show a => Float -> Dist a -> IO a
sample seed dA = do
  let (d,vDs) = select (set0 (filterByProb dA seed))
  xs <- values2list vDs
  putStrLn ("\nPossible values: " ++ show (map distToPair (d:xs)))
  if isEmpty vDs
    then return (value d)
    else do
     sample seed (scale (map distToPair xs))

distToPair :: Dist a -> (a,Float)
distToPair (Dist v (Prob p)) = (v,p)

randomFloat :: IO Float
randomFloat = do
  seed <- getRandomSeed
  let val = head (nextInt seed)
      floatVal = fromInteger (abs val)
  return (truncate floatVal)
 where
  truncate val
    | val < 1.0 = val
    | otherwise = truncate (val / 10.0)

---- -------------------------------------------------------
----  Hoeffd
---- -------------------------------------------------------

hoeffd :: Float -> Float -> Float -> Float -> Float
hoeffd n a b eps =
  2 * exp (- x / y)
 where
  x = 2 * n * (eps ^. 2)
  y = ((b - a) ^. 2)
