{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module WeightedGraph where

import ShowDist
import PFLP
import BayesianNetwork

import List (find,delete)

instance (Ord a,Show a) => Show (Dist a) where
  show = showLight

type WeightedGraph  = [Dist Edge]
-- Edges exist with a certain probability
type Edge   = (Vertex,Vertex)
-- Vertices have unique identifiers
data Vertex = Vertex1 | Vertex2 | Vertex3 | Vertex4 | Vertex5 | Vertex6
 deriving Eq

vertex :: Int -> Vertex
vertex i = case i of
               1 -> Vertex1
               2 -> Vertex2
               3 -> Vertex3
               4 -> Vertex4
               5 -> Vertex5
               6 -> Vertex6
               _ -> error "Vertex does not exist"

edge :: Edge -> Dist Bool
edge edge = case edge of
  (Vertex1,Vertex2) -> bernoulli 0.6
  (Vertex1,Vertex3) -> bernoulli 0.1
  (Vertex2,Vertex5) -> bernoulli 0.4
  (Vertex2,Vertex6) -> bernoulli 0.3
  (Vertex3,Vertex4) -> bernoulli 0.4
  (Vertex4,Vertex5) -> bernoulli 0.8
  (Vertex5,Vertex6) -> bernoulli 0.2
  -- _                 -> bernoulli 0.0

path :: Int -> Int -> Dist Bool
path x y = path' (vertex x) (vertex y)

path' :: Vertex -> Vertex -> Dist Bool
path' x y      = edge (x,y)
path' x y | y /= z = (&&) <$> edge (x,z) <*> path' z y
 where z free

-- Does not work as hoped
--  (due to nondeterminstic definition of `path`):
--  > path 1 5
--  path 1 5
--  (Dist True (Prob 0.24000001))
--  (Dist False (Prob 0.36))
--  (Dist False (Prob 0.16))
--  (Dist False (Prob 0.24))
--  (Dist True (Prob 3.2e-2))
--  (Dist False (Prob 8.0e-3))
--  (Dist False (Prob 4.8000004e-2))
--  (Dist False (Prob 1.2e-2))
--  (Dist False (Prob 0.28800002))
--  (Dist False (Prob 7.2e-2))
--  (Dist False (Prob 0.432))
--  (Dist False (Prob 0.107999995))
--
-- We would like to have the following results:
d1 = (Dist True (Prob 0.2400000001))
d2 = (Dist False (Prob 0.7500000009))
d3 = (Dist True (Prob 3.2e-2))
d4 = (Dist False (Prob 0.968))

-- And finally
orSum = print (foldr (?)
                     (Dist False 0.0)
                     [ (||) <$> v1 <*> v2 | v1 <- [d1,d2]
                                          , v2 <- [d3,d4]
                                          , v1 /= v2])

--  > print orSum
--  False 72.6%
--  True 26.4%

-------------
-- Version 2

-- path'' :: Vertex -> Vertex -> Dist Bool
-- path'' x y      = (||) <$> edge (x,y) <*> (((\e p -> y /= z && e && p) <$> edge (x,z) <*> path'' z y) ? bernoulli 0.0)
--  where z free

-- (<?>) :: Dist Bool -> Dist Bool -> Dist Bool
-- b1 <?> b2 = (||) <$> b1 <*> b2


-- Does not work because the search space is not finite (Vertex Int)
--
-- TODO: Try again with `data Vertex`
--        still doesn't terminate
--
-- g1 :: WeightedGraph
-- g1 = map (\ ((x,y),p) -> dist (vertex x,vertex y) p)
--          [((1,2),0.6)
--          ,((1,3),0.1)
--          ,((2,5),0.4)
--          ,((2,6),0.3)
--          ,((3,4),0.3)
--          ,((4,5),0.8)
--          ,((5,6),0.2)]

-- -- find :: (b -> Bool) -> [Dist (Vertex,Vertex)] -> Maybe b

-- path1 :: WeightedGraph -> (Int,Int) -> Dist Bool
-- path1 g (x,y) = path1' g (vertex x, vertex y)
--  where
--   path1' g e = isElem e g
--   path1' g (x,y) | y /= z && z `elem` map (fst . value) g = (&&) <$> isElem (x,y) g <*> path1' g (z,y)
-- -- path g x y = maybe False (const True) <$> map (filterDist (== (x,y))) g
-- -- path g x y | y /= z = (\mVal bool -> maybe bool (const True) mVal) <$> (find (== (x,z)) <$> g) <*> path g z y
--    where z free

-- isElem :: Edge -> WeightedGraph -> Dist Bool
-- isElem e g = case filter ((==) e . value) g of
--                     []    -> pure False
--                     (e:_) -> const True <$> e
