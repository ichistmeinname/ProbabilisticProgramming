{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module ShowDist
  (showWithRescale, showLight)
 where 

import PFLP
import Distributions (scale')

import Float (i2f,round)
import List (sort,sortBy, maximum)
import SetFunctions (foldValues, mapValues, set0)

showLight :: (Ord a, Show a) => Dist a -> String
showLight = showWithFunction id

showWithRescale :: (Ord a, Show a) => Dist a -> String
showWithRescale = showWithFunction scale

showWithFunction :: (Ord a, Show a) => ([(a,Probability)] -> [(a,Probability)]) -> Dist a -> String
showWithFunction f dist =
  concatMap (\(x,p) -> showR w x ++ ' ' : show p ++ "\n")
            (f (sortP (norm dList)))
 where
  dList = distToList dist
  w = maximum (map (length . show . fst) dList)
  showR n x = let s = show x
              in replicate (n-length s) ' ' ++ s

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

norm :: (Ord a, Eq a) => [(a,Probability)] -> [(a,Probability)]
norm = accumBy (==) . sort

accumBy :: Num b => (a -> a -> Bool) -> [(a,b)] -> [(a,b)]
accumBy _ []   = []
accumBy _ [x]  = [x]
accumBy f ((x,p):(y,q):xs)
  | f x y     = accumBy f ((x,p+q):xs)
  | otherwise = (x,p) : accumBy f ((y,q):xs)

instance Show Probability where
  show (Prob p) = show (i2f (round (p * 10000)) / 100) ++ "%"