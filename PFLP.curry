{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}
module PFLP where

import SetFunctions (Values,mapValues,foldValues,set0,set1,set2,set3,selectValue,chooseValue)
import List (delete,sum,sort,sortBy,maximum)
import Float (exp,i2f,pi,round,sqrt,(^.))
import Combinators (oneOf)

data Probability = Prob Float
  deriving (Eq,Ord)

unP :: Probability -> Float
unP (Prob f) = f

data Dist a = Dist a Probability
  deriving (Eq,Ord)

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

extractDist :: Dist a -> Probability
extractDist fDist =  probability $
  foldValues (\ (Dist x q1) (Dist _ q2) -> Dist x (q1 + q2))
             fail
             (set0 fDist)
  -- probability $
  -- foldValues (\(Dist x q1) d@(Dist _ q2) ->
  --               if p x then Dist x (q1 + q2) else d)
  --            fail
  --            (set0 dist)

filterDist :: (a -> Bool) -> Dist a -> Dist a
filterDist p d@(Dist v _) | p v = d

(<*>) :: Dist (a -> b) -> Dist a -> Dist b
Dist f p <*> Dist x q = Dist (f x) (p*q)

(<$>) :: (a -> b) -> Dist a -> Dist b
(<$>) = mapDist

sequenceA :: [Dist a] -> Dist [a]
sequenceA = traverse id

traverse :: (a -> Dist b) -> [a] -> Dist [b]
traverse f = foldr (liftA2 (:) . f) (pure [])

-- foldA :: (a -> b -> Dist b) -> b -> [a] -> Dist b
-- foldA f e = foldr (\x y -> f x y) e

liftA2 :: (a -> b -> c) -> Dist a -> Dist b -> Dist c
liftA2 f dA dB = f <$> dA <*> dB

pure :: a -> Dist a
pure x = Dist x 1.0

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


singleton :: Dist a -> [Dist a]
singleton x = [x]

enum :: [(a -> Dist a,Float)] -> Dist (a -> Dist a)
enum = flatDist . map (\(f,float) -> Dist f (Prob float))

-- (*.) :: Eq a => Int -> (a -> Dist a) -> a -> Dist a
-- n *. t = chooseValue . mapValues selectValue . set0 . set3 (*..) n t

-- (*..) :: Int -> (a -> Dist a) -> a -> Dist a
-- n *.. t = case n of
--               0 -> certainly
--               1 -> t
--               _ -> t >>: ((n-1) *.. t)

-- -- g potentially non-deterministic!
-- (>>:) :: (a -> Dist a) -> (a -> Dist a) -> a -> Dist a
-- f >>: g = \x -> let d@(Dist y p) = g x
--                     Dist z q     = f y
--                 in Dist z (p*q)

-- Applies a given transition `n`-times
(*.) :: Eq a => Int -> (a -> Dist a) -> a -> Dist a
n *. t = head . (n *.. t)

-- Applies the a given transition `n` times and
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

------------------------------
----- Examples           -----
------------------------------

dice :: Int -> Dist [Int]
dice n | n == 0    = certainly []
       | otherwise = joinWith (:) die (dice (n-1))

dice' :: Int -> Dist [Int]
dice' n | n == 0    = certainly []
        | otherwise = (:) <$> die <*> (dice' (n-1))

die :: Dist Int
die = uniform [1..6]

dieSixes :: Int -> Int -> Probability
dieSixes count rounds =
  extractDist (filterDist ((>= count) . length . filter (== 6)) (dice' rounds))

data Marble = R | G | B
  deriving Eq

rgbExample :: Probability
rgbExample = extractDist (filterDist (== [R,G,B]) (select 3 [R,R,G,G,B]))


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

-- ---------------------------------------------------------
-- Randomisation
-- ---------------------------------------------------------

pick :: Dist a -> a
pick = value . selectValue . set0

random :: (a -> Dist a) -> a -> a
random f = pick . f

-- rDist :: Ord a => [a] -> Dist a
