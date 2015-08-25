{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module ShowDist
  -- (showWithRescale, showLight, showWithFunction, scale)
 where 

import PFLP
import Distributions (scale')

import Float (i2f,round)
import Function (on)
import List (sort,sortBy, maximum, groupBy,sum)
import SetFunctions (foldValues, mapValues, set0)

showLight :: (Ord a, Show a) => Dist a -> String
showLight = showWithFunction (map sumDists)

showWithRescale :: (Ord a, Show a) => Dist a -> String
showWithRescale = showWithFunction (\xss -> scale (map sumDists xss))

showWithFunction :: (Ord a, Show a) => ([[(a,Probability)]] -> [(a,Probability)]) -> Dist a -> String
showWithFunction f dist =
  concatMap (\(x,p) -> showR w x ++ ' ' : show p ++ "\n")
            (sortP (f (norm dList)))
 where
  dList = distToList dist
  w = maximum (map (length . show . fst) dList)
  showR n x = let s = show x
              in replicate (n-length s) ' ' ++ s

sumDists :: [(a,Probability)] -> (a,Probability)
sumDists [] = error "sumDists: No values in distribution"
sumDists xs@((x,_):_) = (x,sum (map snd xs))

scale :: [(a,Probability)] -> [(a,Probability)]
scale = map (onSnd Prob) . scale' . map (onSnd unP)

onSnd :: (a -> b) -> (c,a) -> (c,b)
onSnd f (x,y) = (x, f y)

sortP :: [(a,Probability)] -> [(a,Probability)]
sortP = sortBy (\x y -> snd y <= snd x)

distToList :: Dist a -> [(a,Probability)]
distToList dist =
  foldValues (\xs ys -> xs ++ ys) []
    $ mapValues (\(Dist v p) -> [(v,p)]) (set0 dist)

norm :: (Ord a, Eq a) => [(a,Probability)] -> [[(a,Probability)]]
norm = groupBy (\x y -> fst y == fst x) . sort

-- accumBy :: Num b => (a -> a -> Bool) -> [(a,b)] -> [(a,b)]
-- accumBy _ []   = []
-- accumBy _ [x]  = [x]
-- accumBy f ((x,p):(y,q):xs)
--   | f x y     = accumBy f ((x,p+q):xs)
--   | otherwise = (x,p) : accumBy f ((y,q):xs)

instance Show Probability where
  show (Prob p) = show (i2f (round (p * 10000)) / 100) ++ "%"