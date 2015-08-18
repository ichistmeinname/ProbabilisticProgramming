{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module BayesianNetwork
  ( (=:), (|>), (<|), given, jointProbability, bernoulli, guard )
 where

import List (sum)
import PFLP
import SetFunctions (set0, mapValues, foldValues)
import Distributions (bernoulli)

infixl 5 =:
infixl 4 |>
infixl 4 <|

(=:) :: Eq a => Dist a -> a -> Dist a
dA =: val = filterDist (== val) dA

guard :: Bool -> Dist ()
guard True = pure ()

given :: Eq a => (Dist a,a) -> [Dist a] -> Probability
given (dist,val) = uncurry sumCondDist . condProbability (\() -> dist,val)

(|>) :: Dist a -> (a -> Dist b) -> Dist b
dA |> f = f (value dA)

(<|) :: (a -> Dist b) -> Dist a -> Dist b
(<|) = flip (|>)

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