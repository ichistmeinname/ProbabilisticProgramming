module TreeEvolution where

import PFLP (Dist(..), Probability(..), certainly )
import Distributions (normal)
import SetFunctions (foldValues, mapValues --, set1
                    )
import Combinators (oneOf)

type Height = Int
data Tree = Alive Height | Hit Height | Fallen
  deriving (Eq,Ord,Show)

grow :: Tree -> Dist Tree
grow (Alive h) = normal [Alive k | k <- [h+1,h+2,h+3,h+4,h+5]]

hit :: Tree -> Dist Tree
hit (Alive h) = certainly (Hit h)

fall :: Tree -> Dist Tree
fall _ = certainly Fallen

evolve :: Tree -> Dist Tree
evolve t = case t of
  Alive _ -> unfoldT (enum [(grow,0.9),(hit,0.04),(fall,0.06)]) t
  _       -> certainly t

tree :: Int -> Tree -> Dist Tree
tree n = n *. evolve


-- -----------------------------------------------
--  Auxiliary Functions
-- -----------------------------------------------

-- selectOne :: Eq a => [a] -> Dist (a,[a])
-- selectOne xs = uniform (map (\x -> (x, delete x xs)) ys)
--  where
--   ys = valuesToList (set1 oneOf xs)

-- selectMany :: Eq a => Int -> [a] -> Dist ([a],[a])
-- selectMany n c
--   | n == 0    = certainly ([],c)
--   | otherwise =
--       selectOne c >>= (\ (x,c1) -> selectMany (n-1) c1
--                         >>= \(xs,c2) -> certainly (x:xs,c2))

-- select :: Eq a => Int -> [a] -> Dist [a]
-- select n = mapDist (reverse . fst) . selectMany n

-- Applies a given transition `n`-times
(*.) :: Eq a => Int -> (a -> Dist a) -> a -> Dist a
n *. t = head . (n *.. t)

-- Applies the given transition `n` times and
--  traces each step of the evolution
(*..) :: Int -> (a -> Dist a) -> a -> [Dist a]
n *.. t = case n of
              0 -> singleton . certainly
              1 -> singleton . t
              _ -> t >>: ((n-1) *.. t)

-- Applies a transition function to all entries of a trace
--  that are produced by the given tracing function,
--  thus, yield a trace
(>>:) :: (a -> Dist a) -> (a -> [Dist a]) -> a -> [Dist a]
f >>: g = \x -> let ds@(Dist y p:_) = g x
                    Dist z q        = f y
                in Dist z (p*q) : ds

singleton :: Dist a -> [Dist a]
singleton x = [x]

enum :: [(a -> Dist a,Float)] -> Dist (a -> Dist a)
enum = flatDist . map (\(f,float) -> Dist f (Prob float))

unfoldT :: Dist (a -> Dist a) -> a -> Dist a
unfoldT (Dist f p) x = Dist y (p*q)
 where
  Dist y q = f x

flatDist :: [Dist a] -> Dist a
flatDist = foldr (\d d' -> d ? d') failed

valuesToList :: Values a -> [a]
valuesToList = foldValues (\v acc -> v ++ acc) [] . mapValues (: [])