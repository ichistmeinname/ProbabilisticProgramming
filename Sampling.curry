module Sampling where

import Random (nextIntRange, nextInt, getRandomSeed)

---- -------------------------------------------------------
----  Sampling
---- -------------------------------------------------------

-- filterByProb d@(Dist _ (Prob p)) rFloat | rFloat < p = d

-- sample :: Dist a -> IO a
-- sample dA = do
--   -- xs <- values2list (set0 dA)
--   rnd <- randomFloat
--   let (d,vDs) = select (set0 (filterByProb dA rnd))
--   if isEmpty vDs
--     then return x
--     else sample (foldValues (\Dist v (Prop p) -> Dist (v,p) (Prop p) ? acc) failed (mapValues (\(Dist v (Prop p)

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
