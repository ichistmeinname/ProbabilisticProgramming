{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module PFLP where

import SetFunctions (mapValues,foldValues,set0)
import List (delete,sum)
import Float (i2f,round)

import Combinators

data Probability = Prob Float
unP :: Probability -> Float
unP (Prob f) = f

data Dist a = Dist a Probability

instance Num Probability where
  Prob x + Prob y = Prob (x + y)
  Prob x * Prob y = Prob (x * y)
  fromInteger x = Prob (fromInteger x)

instance Fractional Probability where
  Prob x / Prob y    = Prob (x / y)
  fromFloat x = Prob x

instance Show Probability where
  show (Prob p) = show (i2f (round (p * 1000)) / 10) ++ "%"

value :: Dist a -> a
value (Dist x _) = x

probability :: Dist a -> Probability
probability (Dist _ p) = p

uniform :: [a] -> Dist a
uniform xs = foldr (\x b -> Dist x (1.0 / count) ? b) failed xs
 where
  count = length xs

certainly :: a -> Dist a
certainly x = Dist x 1.0

fail :: Dist a
fail = Dist failed 0.0

die :: Dist Int
die = uniform [1..6]

joinWith :: (a -> b -> c) -> Dist a -> Dist b -> Dist c
joinWith f (Dist x p) (Dist y q) = Dist (f x y) (p*q)

dice :: Int -> Dist [Int]
dice n | n == 0    = certainly []
       | otherwise = joinWith (:) die (dice (n-1))


(>>=) :: Dist a -> (a -> Dist b) -> Dist b
Dist x p >>= f = Dist y (q * p)
 where
  Dist y q = f x

(>@>) :: (a -> Dist b) -> (b -> Dist c) -> a -> Dist c
(>@>) f g = (>>= g) . f

sequence :: [a -> Dist a] -> a -> Dist a
sequence = foldl (>@>) certainly

filterDist :: (a -> Bool) -> Dist a -> Probability
filterDist p dist = probability $
  foldValues (\(Dist x q1) d@(Dist _ q2) ->
                if p x then Dist x (q1 + q2) else d)
             fail
             (set0 dist)

mapDist :: (a -> b) -> Dist a -> Dist b
mapDist f (Dist x p) = Dist (f x) p

selectOne :: Eq a => [a] -> Dist (a,[a])
selectOne xs = uniform [(x, delete x xs)]
 where
  x = oneOf xs

selectMany :: Eq a => Int -> [a] -> Dist ([a],[a])
selectMany n c
  | n == 0    = certainly ([],c)
  | otherwise =
      selectOne c >>= (\ (x,c1) -> selectMany (n-1) c1
                        >>= \(xs,c2) -> certainly (x:xs,c2))

select :: Eq a => Int -> [a] -> Dist [a]
select n = mapDist (reverse . fst) . selectMany n

dieSixes :: Int -> Int -> Probability
dieSixes count rounds =
  filterDist ((>= count) . length . filter (== 6)) (dice rounds)

data Marble = R | G | B
  deriving Eq

rgbExample :: Probability
rgbExample = filterDist (== [R,G,B]) (select 3 [R,R,G,G,B])