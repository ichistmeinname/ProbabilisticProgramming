{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module WeightedGraph where

import ShowDist
import PFLP
import BayesianNetwork

import List (find)

instance (Ord a,Show a) => Show (Dist a) where
  show = showLight

{- Example taken from https://dtai.cs.kuleuven.be/problog/tutorial.html#tut_part1_graphs -}

type WeightedGraph  = [Dist Edge]
-- Edges exist with a certain probability
type Edge   = (Vertex,Vertex)
-- Vertices have unique identifiers
type Vertex = Int

edge :: Edge -> Dist Bool
edge e = case e of
  (1,2) -> bernoulli 0.6
  (1,3) -> bernoulli 0.1
  (2,5) -> bernoulli 0.4
  (2,6) -> bernoulli 0.3
  (3,4) -> bernoulli 0.3
  (4,5) -> bernoulli 0.8
  (5,6) -> bernoulli 0.2
  _     -> bernoulli 0.0

path :: Vertex -> Vertex -> Dist Bool
path x y          = (||) <$> edge (x,y) <*> ((&&) <$> edge (x,z) <*> path z y)
 where z free

-- edge :: Edge -> Float -> Dist Edge
-- edge vertexPair prob = mkDist vertexPair prob

-- g1 :: WeightedGraph
-- g1 = map (uncurry edge) [((1,2),0.6)
--                         ,((1,3),0.1)
--                         ,((2,5),0.4)
--                         ,((2,6),0.3)
--                         ,((3,4),0.3)
--                         ,((4,5),0.8)
--                         ,((5,6),0.2)]

-- find :: (b -> Bool) -> [Dist (Vertex,Vertex)] -> Maybe b

-- path :: WeightedGraph -> Edge -> Dist Bool
-- path g edge = isElem edge g
-- path g (x,y) | y /= z && z `elem` map (fst . value) g = (&&) <$> isElem (x,y) g <*> path g (z,y)
-- -- path g x y = maybe False (const True) <$> map (filterDist (== (x,y))) g
-- -- path g x y | y /= z = (\mVal bool -> maybe bool (const True) mVal) <$> (find (== (x,z)) <$> g) <*> path g z y
--  where z free

-- isElem :: Edge -> WeightedGraph -> Dist Bool
-- isElem edge g = case filter ((==) edge . value) g of
--                     []    -> pure False
--                     (e:_) -> const True <$> e
