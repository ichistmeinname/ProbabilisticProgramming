{-

    The same problem was modelled in other languages too

    Church: https://www.uni-oldenburg.de/en/computingscience/lcs/probabilistic-programming/church-a-probabilistic-scheme/example-6a-bayesian-network-student-model-with-evidence/
    Figaro: https://www.uni-oldenburg.de/en/computingscience/lcs/probabilistic-programming/figaro-yet-another-probabilistic-programming-language/example-5-6-bayesian-network-student-model/

-}

module StudentModell where

import BayesianNetwork
import PFLP

intelligence, difficulty :: Dist Bool
intelligence = bernoulli 0.3
difficulty   = bernoulli 0.4

sat :: Dist Bool -> Dist Bool
sat vIntelligence = vIntelligence |> (bernoulli . f)
 where
  f True  = 0.8
  f False = 0.05

grade :: Dist Bool -> Dist Bool -> Dist Bool
grade vIntelligence vDifficulty =
  vIntelligence ||| vDifficulty |> (bernoulli . uncurry f)
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
  in jointProbability [i',d',g',s',l']

letterExample' i d g s l bools =
  jointProbability (zipWith (=:) [i, d, g i d, s i, l (g i d)] bools)

-- P(i^1,d^0,g^1,s^1,l^0) = 0.00576
letterExample   = letterOfRecommendation True False True True False

-- P(L=True,G=True) = 0.6
-- ex6a = let g' = grade intelligence difficulty =: True
--        in letter g' =: True
letterWhenGrade gBool =
  let i' = intelligence
      d' = difficulty
      g' = grade i' d' =: True
      s' = sat i'
      l' = letter g'
  in (l',True) `given` [g',s',d',i']

letterWhenGrade' gBool =
  let i' = intelligence
      d' = difficulty
      g' = grade i' d' =: gBool
      s' = sat i'
      l' = letter g'
  in l' =: True

-- letterOfRecommendation' bools =
--   let g = grade i d
--       i = intelligence
--       d = difficulty
--       s = sat i
--   in sequenceA $
--       zipWith (=:) [i,d,g,s,letter g]
--                    bools

-- letterExample'  = letterOfRecommendation' [True,False,True,True,False]


-- letterOfRecommendation'' i d g s l bools =
--   sequenceA $
--     zipWith (=:) [i,d,g',s i,l g']
--                  bools
--  where
--   g' = g i d

-- letterExample'' =
--   letterOfRecommendation'' intelligence
--                            difficulty
--                            grade
--                            sat
--                            letter
--                            [True,False,True,True,False]


-- letterOfRecommendation''' bools =
--   let i' = intelligence
--       d' = difficulty
--       g' = grade i' d'
--       s' = sat i'
--       l' = letter g'
--   in filterDist (== bools) (sequenceA [i',d',g',s',l'])

-- letterExample'''   = letterOfRecommendation''' [True,False,True,True,False]
