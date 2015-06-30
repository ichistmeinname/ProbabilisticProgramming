module Dice where

import PFLP ( Dist(..), Probability (..)
           , pure, filterDist, sumDist )
import Distributions (uniform, uniform')

rollDices :: Int -> (_ -> Dist a) -> Dist [a]
rollDices n = replicateWith n _

die :: Dist Int
die = uniform [1..6]

dieSixes :: Int -> Int -> (a -> Bool) -> Dist a -> Probability
dieSixes count rounds cond aDice = sumDist
  (filterDist ((>= count) . length . filter cond)
              (rollDices rounds (\() -> aDice)))

dieSixes' :: Int -> Int -> (a -> Bool) -> (_ -> Dist a) -> Probability
dieSixes' count rounds cond aDice = sumDist
  (filterDist (atLeastN count cond)
  -- (filterDist ((>= count) . length . filter cond)
              (rollDices rounds aDice))

atLeastN :: Int -> (a -> Bool) -> [a] -> Bool
atLeastN = atLeastN' 0
atLeastN' n m _ []     = n >= m
atLeastN' n m p (x:xs)
  | n >= m    = True
  | otherwise = atLeastN' (next n) m p xs
 where
  next | p x       = (+ 1)
       | otherwise = id

data Dice = One | Two | Three | Four | Five | Six
  deriving (Bounded,Enum,Eq,Ord,Show)

die' :: _ -> Dist Dice
die' _ = uniform' _

-- -----------------------------------------------
--  Auxiliary Functions
-- -----------------------------------------------

replicateWith :: Int -> a -> (a -> Dist b) -> Dist [b]
replicateWith n v fd
  | n == 0    = certainly []
  | otherwise = (:) <$> fd v <*> replicateWith (n - 1) v fd