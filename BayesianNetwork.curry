{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module BayesianNetwork where

import List (sum)
import PFLP
import Distributions (bernoulli)

(=:) :: Eq a => Dist a -> a -> Dist a
dA =: val = filterDist (== val) dA

given :: Eq a => (Dist a,a) -> [Dist a] -> Probability
given (dist,val) = uncurry sumCondDist . condProbability (\() -> dist,val)


-- Auxiliary functions

jointProbability :: [Dist a] -> Dist [a]
jointProbability = sequenceA

condProbability :: Eq a => (() -> Dist a,a)
                        -> [Dist a]
                        -> ([Probability],[Probability])
condProbability (distA,valA) dAs = (probsN, probsD)
 where
  probsD    = collectProbs (calc (distA ()))
  probsN    = collectProbs (calc distA')
  distA'    = distA () =: valA
  dists'    = jointProbability dAs
  calc dist = concat <$>
    jointProbability [(\x -> [x]) <$> dist, dists']

collectProbs :: Dist a -> [Probability]
collectProbs dist =
  foldValues (++)
             []
             (mapValues (flip (:) [] . probability) (set0 dist))

sumConditional :: Eq a => (Dist a,a) -> [Dist a] -> Probability
sumConditional (dist,val) dists =
  uncurry sumCondDist (condProbability (\ () -> dist,val) dists)

sumCondDist :: [Probability] -> [Probability] -> Probability
sumCondDist probN probD = sum probN / sum probD