{-
   Problem seen at ProbLog2 Tutorial:
     https://dtai.cs.kuleuven.be/problog/tutorial.html#tut_part1_packing

-}

module BagPack where

import PFLP
import BayesianNetwork

data Item = Skis | Boots | Helmet | Gloves

weight :: Item -> Int
weight Skis   = 6
weight Boots  = 4
weight Helmet = 3
weight Gloves = 2

pack :: Item -> Dist Bool
pack item = bernoulli (1.0 / w)
 where
  w = fromInteger (weight item)

excess :: Int -> Dist [Bool]
excess = excess' [Skis, Boots, Helmet, Gloves]
 where
  excess' []     limit | limit < 0 = pure []
  excess' (i:is) limit = (:) <$> (pack i =: True) <*> excess' is (limit - weight i)
  excess' (i:is) limit = (:) <$> (pack i =: False) <*> excess' is limit

