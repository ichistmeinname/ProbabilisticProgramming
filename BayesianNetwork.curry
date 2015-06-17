{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module BayesianNetwork where

import PFLP

-- `f` calculates a new probability dependent on the values
--   of dA and dB
(|>) :: Dist a -> (a -> Dist b) -> Dist b
Dist vA _ |> f = f vA

(|||) :: Dist a -> Dist b -> Dist (a,b)
(|||) = liftA2 (,)

(=:) :: Eq a => Dist a -> a -> Dist a
dA =: val = filterDist (== val) dA

bernoulli :: Float -> Dist Bool
bernoulli v = Dist True (Prob v) ? Dist False (Prob (1.0 - v))