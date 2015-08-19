{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module Markov where

import ShowDist (showLight)
import PFLP
import BayesianNetwork

data Condition = Sunny | Rainy | Foggy
 deriving (Eq,Show,Ord)

instance (Ord a, Show a) => Show (Dist a) where
  show = showLight

type Day = Int

condition :: Float -> Float -> Float -> Dist Condition
condition sunny rainy foggy = mkDist Sunny (Prob sunny) ? mkDist Rainy (Prob rainy) ? mkDist Foggy (Prob foggy)

weather' :: Condition -> Dist Condition
weather' Sunny = condition 0.8 0.05 0.15
weather' Rainy = condition 0.2 0.6 0.2
weather' Foggy = condition 0.2 0.3 0.5

weather :: Condition -> Day -> Dist Condition
weather cond day
  | day == 0 = condition 0.5 0.5 0.0
  | day  > 0 && cond == Sunny = condition 0.6 0.4 0.0 >>>= \c -> weather c (day - 1)
  | day  > 0 && cond == Rainy = condition 0.2 0.8 0.0 >>>= \c -> weather c (day - 1)

weatherJ :: Condition -> Day -> Dist [Condition]
weatherJ cond day
  | day == 0 = pure [cond] -- condition 0.5 0.5 0.0 >>>= \c -> pure [c]
  | day  > 0 && cond == Sunny = condition 0.6 0.4 0.0 >>>= \c -> weatherJ c (day - 1) >>>= \cs -> pure (Sunny:cs)
  | day  > 0 && cond == Rainy = condition 0.2 0.8 0.0 >>>= \c -> weatherJ c (day - 1) >>>= \cs -> pure (Rainy:cs)

weather'' :: Condition -> Day -> Dist Condition
weather'' cond day
  | day == 0 = condition 0.5 0.5 0.0
  | day  > 0 && cond == Sunny = condition 0.6 0.4 0.0
  | day  > 0 && cond == Rainy = condition 0.2 0.8 0.0

sunny10 = (weatherJ Sunny 2)

q1 = weather'' Sunny 1 >>>= \c1 ->
     weather'' c1    1 >>>= \c2 ->
     weather'' c2    0


q2 = weather' Sunny =: Sunny>>>= \c1  ->
     weather' c1 =: Rainy
q3 = weather' Foggy >>>= \c1 ->
     weather' c1 =: Rainy

last (_ ++ [x]) = x