{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module BayesianNetwork where

import PFLP

-- `f` calculates a new probability dependent on the values
--   of dA and dB
(|>) :: Dist a -> (a -> Dist b) -> Dist b
Dist vA _ |> f = f vA

(|||) :: Dist a -> Dist b -> Dist (a,b)
(|||) = liftA2 (,)

(=:) :: Eq a => Dist a -> a -> Dist a
dA =: val = filterDist (== val) dA

bernoulli :: Float -> Dist Bool
bernoulli v = Dist True (Prob v) ? Dist False (Prob (1.0 - v))

intelligence, difficulty :: Dist Bool
intelligence = bernoulli 0.3
difficulty = bernoulli 0.4

sat :: Dist Bool -> Dist Bool
sat vIntelligence = vIntelligence |> (bernoulli . f)
 where
  f True  = 0.8
  f False = 0.05

-- (True,True) 0.12
-- (True,False) 0.18
-- (False,True) 0.28
-- (False,False) 0.42
grade :: Dist Bool -> Dist Bool -> Dist Bool
grade vIntelligence vDifficulty =
  (vIntelligence ||| vDifficulty) |> (bernoulli . uncurry f)
 where
  f True  True  = 0.5
  f True  False = 0.1
  f False True  = 0.95
  f False False = 0.7

letter :: Dist Bool -> Dist Bool
letter vGrade = vGrade |> (bernoulli . f)
 where
  f True  = 0.6
  f False = 0.1

letterOfRecommendation iBool dBool gBool sBool lBool =
  let i' = intelligence =: iBool
      d' = difficulty =: dBool
      g' = grade i' d' =: gBool
      s' = sat i' =: sBool
      l' = letter g' =: lBool
  in sequenceA [i',d',g',s',l']

letterOfRecommendation' bools =
  let g = grade i d
      i = intelligence
      d = difficulty
      s = sat i
  in sequenceA $
      zipWith (=:) [i,d,g,s,letter g]
                   bools

letterOfRecommendation'' i d g s l bools =
  sequenceA $
    zipWith (=:) [i,d,g',s i,l g']
                 bools
 where
  g' = g i d

letterOfRecommendation''' bools =
  let i' = intelligence
      d' = difficulty
      g' = grade i' d'
      s' = sat i'
      l' = letter g'
  in filterDist (== bools) (sequenceA [i',d',g',s',l'])

-- P(i^1,d^0,g^1,s^1,l^0) = 0.00576
letterExample   = letterOfRecommendation True False True True False
letterExample'  = letterOfRecommendation' [True,False,True,True,False]
letterExample'' =
  letterOfRecommendation'' intelligence
                           difficulty
                           grade
                           sat
                           letter
                           [True,False,True,True,False]
letterExample'''   = letterOfRecommendation''' [True,False,True,True,False]


ex6a = let g' = grade intelligence difficulty =: True
       in letter g' =: True