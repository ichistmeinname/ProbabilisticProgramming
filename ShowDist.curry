{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module ShowDist where

import PFLP
import Float (i2f,round)
import List (sort,sortBy)
import Combinators (oneOf)

instance (Ord a, Show a) => Show (Dist a) where
  show = show . toShowD

data ShowDist a = ShowDist [(a,Probability)]
  deriving (Eq,Ord)

instance (Ord a, Show a) => Show (ShowDist a) where
  show (ShowDist xs) = concatMap (\(x,p)-> showR w x ++ ' ' : show p ++ "\n") (sortP (norm xs))
   where
    w = maximum (map (length . show . fst) xs)
    showR n x = replicate (n-length s) ' ' ++ s
      where s = show x

sortP :: [(a,Probability)] -> [(a,Probability)]
sortP = sortBy (\x y -> snd y <= snd x)

toShowD :: Dist a -> ShowDist a
toShowD dist =
  foldValues (\ (ShowDist xs) (ShowDist ys) -> ShowDist (xs ++ ys)) (ShowDist [])
    $ mapValues (\(Dist v p) -> ShowDist [(v,p)]) (set0 dist)

norm :: (Ord a, Eq a) => [(a,Probability)] -> [(a,Probability)]
norm = accumBy (==) . sort

accumBy :: Num b => (a -> a -> Bool) -> [(a,b)] -> [(a,b)]
accumBy _ []   = []
accumBy _ [x]  = [x]
accumBy f ((x,p):(y,q):xs)
  | f x y     = accumBy f ((x,p+q):xs)
  | otherwise = (x,p) : accumBy f ((y,q):xs)

instance Show Probability where
  show (Prob p) = show (i2f (round (p * 1000)) / 10) ++ "%"