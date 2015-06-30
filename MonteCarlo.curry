module MonteCarlo where

import PFLP

---- -------------------------------------------------------
----  Monte-Carlo-Simulation
---- -------------------------------------------------------

-- takeSample :: Int -> Dist [Bool]
-- takeSample n = map (const (liftA2 isInsideCircle x y)) [1..n]
--  where
--   x = uniform' (_ :: Bool)
--   y = uniform' (_ :: Bool)
--   isInsideCircle v1 v2 = v1 * v1 + v2 * v2 <= 1.0

-- evaluateSamples :: Dist [Bool] -> Float
-- evaluateSamples samples = inside / total
--  where
--   inside = sum (map (\x -> if x then 1.0 else 0.0) samples)
--   total  = length samples
