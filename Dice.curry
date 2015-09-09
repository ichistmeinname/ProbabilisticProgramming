{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}
module Dice where

import PFLP ( Dist(..), Probability (..)
           , pure, filterDist, sumDist
           , (<$>), (<*>))
import Distributions (uniform, uniform')

rollDices :: Int -> (_ -> Dist a) -> Dist [a]
rollDices n = replicateWith n _

die :: Dist Int
die = uniform [1..6]

-- probability to roll `value` `rounds` times
allTimesN :: Eq a => a -> Int -> Dist a -> Probability
allTimesN value rounds aDice =
  let d = aDice
  in dieNTimes (== rounds) rounds (== value) (\ () -> d)
  -- sumDist
  -- (filterDist ((== rounds) . length . filter (== value))
  --             (rollDices rounds (\ () -> aDice)))

-- probability to roll `pValue x` `pCount y` times after `rounds` times
dieNTimes :: (Int -> Bool) -> Int -> (a -> Bool) -> (() -> Dist a) -> Probability
dieNTimes pCount rounds pValue aDice = sumDist
  (filterDist (pCount . length . filter pValue)
              (rollDices rounds aDice))

data Dice = One | Two | Three | Four | Five | Six
  deriving (Bounded,Enum,Eq,Ord,Show)

die' :: Dist Dice
die' = uniform' _


-- -----------------------------------------------
--  Auxiliary Functions
-- -----------------------------------------------

replicateWith :: Int -> a -> (a -> Dist b) -> Dist [b]
replicateWith n v fd
  | n == 0    = pure []
  | otherwise = (:) <$> fd v <*> replicateWith (n - 1) v fd



{- Tests

-- uses commited-choice to reduce search tree
dieNTimes (== 9) 9 (==Six) (\() -> die')
(Prob 9.9229055e-8)

allTimesN Six 9 die'
(Prob 9.9229055e-8)
-}