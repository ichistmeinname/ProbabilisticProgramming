{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module Probabilistic where
import SetFunctions (set0, mapValues, foldValues)

data Probability = Prob Float
data Dist a = Dist a Probability

sumDist :: Dist a -> Probability
sumDist dist = foldValues (\ (Prob p1) (Prob g) -> Prob (p1 + g))
                          (Prob 0.0)
                          (mapValues (\Dist _ p -> p)
                                     (set0 dist))

filterDist :: (a -> Bool) -> Dist a -> Dist a
filterDist p (Dist val prob)
  | p val     = Dist val prob

pure :: a -> Dist a
pure val = Dist val (Prob 1.0)

(<*>) :: Dist (a -> b) -> Dist a -> Dist b
Dist fAB (Prob p1) <*> Dist valA (Prob p2) = Dist (fAB valA) (Prob (p1 * p2))