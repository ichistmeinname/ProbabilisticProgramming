{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module PFLP where

import SetFunctions (Values,mapValues,foldValues,set0,set1,selectValue)
import List (delete,sum)
import Float (exp,i2f,pi,round,sqrt,(^.))
import Combinators (oneOf)

data Probability = Prob Float
unP :: Probability -> Float
unP (Prob f) = f

data Dist a = Dist a Probability

type Spread a = [a] -> Dist a

instance Num Probability where
  Prob x + Prob y = Prob (x + y)
  Prob x * Prob y = Prob (x * y)
  fromInteger x = Prob (fromInteger x)

instance Fractional Probability where
  Prob x / Prob y    = Prob (x / y)
  fromFloat x = Prob x

instance Show Probability where
  show (Prob p) = show (i2f (round (p * 1000)) / 10) ++ "%"

value :: Dist a -> a
value (Dist x _) = x

probability :: Dist a -> Probability
probability (Dist _ p) = p

flatDist :: [Dist a] -> Dist a
flatDist = foldr (\d d' -> d ? d') failed

uniform :: [a] -> Dist a
uniform xs = flatDist $ map (flip Dist (1.0 / count)) xs
 where
  count = fromInteger (length xs)

scale :: [(a,Float)] -> Dist a
scale xs = foldr (\(x,p) acc -> Dist x (Prob (p/q)) ? acc) failed xs
 where
  q = sum (map snd xs)

normal :: Spread a
normal = shape (normalCurve 0.5 0.5)

normalCurve :: Float -> Float -> Float -> Float
normalCurve mean stddev x = 1 / sqrt (2 * pi) * exp (-1/2 * u^.2)
 where
  u = (x - mean) / stddev

shape :: (Float -> Float) -> Spread a
shape _ [] = fail
shape f xs = scale (zip xs ps)
 where
  incr = 1 / i2f ((length xs) - 1)
  ps = map f (iterate (+incr) 0)

certainly :: a -> Dist a
certainly x = Dist x 1.0

fail :: Dist a
fail = Dist failed 0.0

joinWith :: (a -> b -> c) -> Dist a -> Dist b -> Dist c
joinWith f (Dist x p) (Dist y q) = Dist (f x y) (p*q)

dice :: Int -> Dist [Int]
dice n | n == 0    = certainly []
       | otherwise = joinWith (:) die (dice (n-1))


(>>=) :: Dist a -> (a -> Dist b) -> Dist b
Dist x p >>= f = Dist y (q * p)
 where
  Dist y q = f x

unfoldT :: Dist (a -> Dist a) -> a -> Dist a
unfoldT (Dist f p) x = Dist y (p*q)
 where
  Dist y q = f x

(>@>) :: (a -> Dist b) -> (b -> Dist c) -> a -> Dist c
(>@>) f g = (>>= g) . f

sequence :: [a -> Dist a] -> a -> Dist a
sequence = foldl (>@>) certainly

filterDist :: (a -> Bool) -> Dist a -> Probability
filterDist p dist = probability $
  foldValues (\(Dist x q1) d@(Dist _ q2) ->
                if p x then Dist x (q1 + q2) else d)
             fail
             (set0 dist)

mapDist :: (a -> b) -> Dist a -> Dist b
mapDist f (Dist x p) = Dist (f x) p

selectOne :: Eq a => [a] -> Dist (a,[a])
selectOne xs = uniform (map (\x -> (x, delete x xs)) ys)
 where
  ys = valuesToList (set1 oneOf xs)

selectMany :: Eq a => Int -> [a] -> Dist ([a],[a])
selectMany n c
  | n == 0    = certainly ([],c)
  | otherwise =
      selectOne c >>= (\ (x,c1) -> selectMany (n-1) c1
                        >>= \(xs,c2) -> certainly (x:xs,c2))

select :: Eq a => Int -> [a] -> Dist [a]
select n = mapDist (reverse . fst) . selectMany n

valuesToList :: Values a -> [a]
valuesToList = foldValues (\v acc -> v ++ acc) [] . mapValues (: [])

------------------------------
----- Examples           -----
------------------------------

die :: Dist Int
die = uniform [1..6]

dieSixes :: Int -> Int -> Probability
dieSixes count rounds =
  filterDist ((>= count) . length . filter (== 6)) (dice rounds)

data Marble = R | G | B
  deriving Eq

rgbExample :: Probability
rgbExample = filterDist (== [R,G,B]) (select 3 [R,R,G,G,B])


type Height = Int
data Tree = Alive Height | Hit Height | Fallen
  deriving (Eq,Show)

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

enum :: [(a -> Dist a,Float)] -> Dist (a -> Dist a)
enum = flatDist . map (\(f,float) -> Dist f (Prob float))

tree :: Int -> Tree -> Dist Tree
tree n = n *. evolve

(*.) :: Eq a => Int -> (a -> Dist a) -> a -> Dist a
-- (*.) x f y = failed
(n *. t) val = selectValue  (set1 (n *.. t) val)

(*..) :: Int -> (a -> Dist a) -> a -> Dist a
0 *.. _ = certainly
1 *.. t = t
n *.. t = t >>: ((n-1) *.. t)

(>>:) :: (a -> Dist a) -> (a -> Dist a) -> a -> Dist a
f >>: g = \x -> let d@(Dist y p) = g x
                    Dist z q     = f y
                in Dist z (p*q) ? d
