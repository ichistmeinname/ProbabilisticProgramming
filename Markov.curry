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
condition sunny rainy foggy = dist Sunny sunny ? dist Rainy rainy ? dist Foggy foggy

-- Example taken from https://dtai.cs.kuleuven.be/problog/tutorial.html#tut_part1_HMM
weather :: Condition -> Day -> Dist Condition
weather cond day = w' cond 0 day =: cond
 where
  w' cond day max
    | day == 0 = condition 0.5 0.5 0.0 >>>= \c -> w' c (day+1) max
    | day <= max = (case cond of
                       Sunny -> condition 0.6 0.4 0.0
                       Rainy -> condition 0.2 0.8 0.0)
                    >>>= \c -> w' c (day+1) max
    | day > max  = pure cond

-- The probabilities need to be accumulated in reverse order (see `weather` above)
-- weather :: Condition -> Day -> Dist Condition
-- weather cond day = w' cond day day
--  where
--   w' cond 
--     | day == 0 = condition 0.5 0.5 0.0
--     | day  > 0 && cond == Sunny =
--        condition 0.6 0.4 0.0 >>>= \c -> weather c (day - 1)
--     | day  > 0 && cond == Rainy =
--        condition 0.2 0.8 0.0 >>>= \c -> weather c (day - 1)

--  Rainy: 64%
--  Sunny: 36%
q1 = weather Sunny 2

-- Rainy: 66,66%
-- Sunny: 33,33%
q2 = weather Sunny 10

-- Example taken from http://cogsys.imm.dtu.dk/teaching/04364/ex10/intro_HMM_01.pdf
weather' :: Condition -> Dist Condition
weather' Sunny = condition 0.8 0.05 0.15
weather' Rainy = condition 0.2 0.6 0.2
weather' Foggy = condition 0.2 0.3 0.5

-- Given that today is sunny, what's the probability that
--  tomorrow is sunny and the after is rainy?
--
-- We need three events X = {x1,x2,x3}
--    p(x2 = Sunny, x3 = Rainy | x1 = Sunny)
--  = p(x3 = Rainy | x2 = Sunny, x1 = Sunny) * p(x2 = Sunny | x1 = Sunny)
--  { Markov Property }
--  = p(x3 = Rainy | xs = Sunny) * p(x2 = Sunny | x1 = Sunny)
--  = 0.05 * 0.8
--  = 0.04
q3 = weather' Sunny =: Sunny >>>= \c ->
     weather' c =: Rainy

-- Given that today is foggy, what's the probability that
--  it will be rainy two days from now?
--
-- Again, wee need three events X = {x1,x2,x3}
--    p(x3 = Rainy | x1 = Foggy)
--  = Sum(x2={Sunny,Rainy,Foggy}) p(x3 = Rainy, x2 | x1 = Foggy)
--  { Markov Property }
--  = Sum(x2={Sunny,Rainy,Foggy}) p(x3 = Rainy | x2) * p (x2 | x1 = Foggy)
--  = 0.3 * 0.5 + 0.6 * 0.3 + 0.05 * 0.2
--  = 0.34
q4 = weather' Foggy >>>= \c1 ->
     weather' c1 =: Rainy