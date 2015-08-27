{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module RandomWalk where

import PFLP
import BayesianNetwork
import Distributions (uniform)
import ShowDist

instance (Ord a,Show a) => Show (Dist a) where
  show = showLight

data State = Zero | One | Two | Three | Four | Five | Six | Seven
  deriving (Bounded, Enum, Eq, Ord, Show)

allStates :: [State]
allStates = [minBound .. maxBound]

stateToPos :: State -> Int
stateToPos Zero  = 0
stateToPos One   = 1
stateToPos Two   = 2
stateToPos Three = 3
stateToPos Four  = 4
stateToPos Five  = 5
stateToPos Six   = 6
stateToPos Seven = 7

data Obs = L | R
 deriving (Eq,Show)

nstates = 7

transProb :: State -> [(State,Float)]
transProb st = case st of
                   Zero  -> [(Zero,0.7),(One,0.3)]
                   Seven -> [(Seven,0.7),(Six,0.3)]
                   _     -> [(st,0.4),(next st,0.3),(prev st,0.3)]
next Zero  = One
next One   = Two
next Two   = Three
next Three = Four
next Four  = Five
next Five  = Six
next Six   = Seven
prev One   = Zero
prev Two   = One
prev Three = Two
prev Four  = Three
prev Five  = Four
prev Six   = Five

evolve :: State -> Dist State
evolve st = foldr (\t1 acc -> uncurry dist t1 ? acc) (uncurry dist t) trans
 where
  (t:trans) = transProb st

observe :: State -> Dist Obs
observe st = dist L prob ? dist R (1 - prob)
 where
-- The probability that it observes `L` in position k is 1-(k-1)/n
  prob = abs (fromInteger (1 - (stateToPos st - 1)) / fromInteger (nstates))

run :: Int -> (State -> Int -> Dist ()) -> Dist State
run n evidence =
 let st = if n == 1
            then uniform allStates
            else concatDist (evolve <$> (run (n-1) evidence))
  in (\s -> evidence s n) <$> st >>>= \_ -> st

query i = run i ev

ev :: State -> Int -> Dist ()
ev st n =
  observe st >>>= \ oSt ->
  if n == 5 && oSt == L
     then uncertainly ()
     else pure ()

concatDist (Dist (Dist y p') p) = Dist y p