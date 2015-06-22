{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module BayesianNetwork where

import List (sum)
import SetFunctions (set0,mapValues,foldValues)
import PFLP

-- `f` calculates a new probability dependent on the values
--   of dA and dB
(|>) :: Dist a -> (a -> Dist b) -> Dist b
Dist vA _ |> f = f vA

(|||) :: Dist a -> Dist b -> Dist (a,b)
(|||) = liftA2 (,)

(=:) :: Eq a => Dist a -> a -> Dist a
dA =: val = filterDist (== val) dA

infixl 4 |>
infixl 5 |||

bernoulli :: Float -> Dist Bool
bernoulli v = Dist True (Prob v) ? Dist False (Prob (1.0 - v))

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

sumDist :: Dist a -> Probability
sumDist d = foldValues (+)
                       (Prob 0.0)
                       (mapValues (probability) (set0 d))

given :: Eq a => (Dist a,a) -> [Dist a] -> Probability
given (dist,val) = uncurry sumCondDist . condProbability (\() -> dist,val)