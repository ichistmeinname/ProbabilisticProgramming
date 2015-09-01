{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module Distributions where

import PFLP (Dist(), Probability(..), dist)
import List (sum)
import Float (exp,i2f,pi,sqrt,(^.))

uniform :: [a] -> Dist a
uniform xs = uniformInterval (foldr1 (?) xs) count
 where
  count = length xs

bernoulli :: Float -> Dist Bool
bernoulli v = dist True v ? dist False (1.0 - v)

scale :: [(a,Float)] -> Dist a
scale xs = foldr (\(x,p) acc -> dist x (p/q) ? acc) failed xs
 where
  q = sum (map snd xs)

scale' :: [(a,Float)] -> [(a,Float)]
scale' xs = foldr (\(x,p) acc -> (x,p/q) : acc) [] xs
 where
  q = sum (map snd xs)

normal :: [a] -> Dist a
normal = shape (normalCurve 0.5 0.5)

normalCurve :: Float -> Float -> Float -> Float
normalCurve mean stddev x = 1 / sqrt (2 * pi) * exp (-1/2 * u^.2)
 where
  u = (x - mean) / stddev

shape :: (Float -> Float) -> [a] -> Dist a
shape f xs = scale (zip xs ps)
 where
  incr = 1 / i2f ((length xs) - 1)
  ps = map f (iterate (+incr) 0)

uniform' :: (Bounded a, Enum a) => a -> Dist a
uniform' val = uniformInterval val count
 where
  count = length (enumValues val)

enumValues :: (Bounded a, Enum a) => a -> [a]
enumValues _ = [minBound .. maxBound]

uniformInterval :: a -> Int -> Dist a
uniformInterval val = dist val . (1.0 /) . fromInteger
