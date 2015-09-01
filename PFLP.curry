{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}
module PFLP where

import SetFunctions (foldValues,set0)

infixl 4 <*>
infixl 4 <$>
infixl 4 >>>=

data Probability = Prob Float
  deriving (Eq,Ord)

prob :: Float -> Probability
prob val
  | val <= 1.0 && val >= 0.0 = Prob val
  | otherwise  = error "prob: the probability needs to range between 0.0 and 1.0"

data Dist a = Dist a Probability
  deriving (Eq,Ord)

dist :: a -> Float -> Dist a
dist x prob
  | prob <= 1.0 && prob >= 0.0 = Dist x (Prob prob)
  | otherwise   = error "dist: the probability needs to range between 0.0 and 1.0"

sumDist :: Dist a -> Probability
sumDist dists = probability $
  foldValues (\ (Dist x q1) (Dist _ q2) -> Dist x (q1 + q2))
             fail
             (set0 dists)

filterDist :: (a -> Bool) -> Dist a -> Dist a
filterDist p d@(Dist v _) | p v = d

pure :: a -> Dist a
pure x = Dist x 1.0

(<*>) :: Dist (a -> b) -> Dist a -> Dist b
Dist f p <*> Dist x q = Dist (f x) (p*q)

(>>>=) :: Dist a -> (a -> Dist b) -> Dist b
Dist a p >>>= f =
  let Dist b p' = f a
  in Dist b (p * p')

-- ----------------------
--  Auxiliary Functions
-- ----------------------

-- Applicative "instance"

(<$>) :: (a -> b) -> Dist a -> Dist b
(<$>) f dA = pure f <*> dA

(*>) :: Dist a -> Dist b -> Dist b
dA *> dB = const id <$> dA <*> dB

(<*) :: Dist a -> Dist b -> Dist a
dA <* dB = const <$> dA <*> dB

sequenceA :: [Dist a] -> Dist [a]
sequenceA = traverse id

traverse :: (a -> Dist b) -> [a] -> Dist [b]
traverse f = foldr (liftA2 (:) . f) (pure [])

liftA2 :: (a -> b -> c) -> Dist a -> Dist b -> Dist c
liftA2 f dA dB = f <$> dA <*> dB


-- Num instance

instance Num Probability where
  Prob x + Prob y = prob (x + y)
  Prob x * Prob y = prob (x * y)
  fromInteger x = prob (fromInteger x)

instance Fractional Probability where
  Prob x / Prob y    = prob (x / y)
  fromFloat x = prob x

-- Smart constructors

certainly :: a -> Dist a
certainly x = Dist x 1.0

uncertainly :: a -> Dist a
uncertainly x = Dist x 0.0

fail :: Dist a
fail = Dist failed 0.0


-- Selector functions

unP :: Probability -> Float
unP (Prob f) = f

value :: Dist a -> a
value (Dist x _) = x

probability :: Dist a -> Probability
probability (Dist _ p) = p