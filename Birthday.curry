{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

{-

  Very bad example; search space is too large!

-}

module Birthday where

import Distributions (uniform)
import PFLP

birthday :: Int -> Dist Int
birthday = \_ -> uniform [1..366]

pairEqual :: Int -> Int -> Int -> Dist Bool
pairEqual max p1 p2
  | p1 > max = pure False
  | p2 > max = pairEqual max (p1 + 1) (p1 + 2)
  | birthday p1 <==> birthday p2 = b