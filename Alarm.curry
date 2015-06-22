{-
   Values used from ProbLog2 Tutorial:
     https://dtai.cs.kuleuven.be/problog/tutorial.html

   The same model is used in different languages as well:

     BLOG: http://patmos.banatao.berkeley.edu:8080
       (here: "my values" yield different results in BLOG -- weird?)

-}
module Alarm where

import BayesianNetwork
import PFLP

earthquake :: Dist Bool
earthquake = bernoulli 0.002

burglary :: Dist Bool
burglary = bernoulli 0.001

alarm :: Dist Bool -> Dist Bool -> Dist Bool
alarm vBurglary vEarthquake =
  vBurglary ||| vEarthquake |> (bernoulli . uncurry f)
 where
  f True  True  = 0.95
  f True  False = 0.94
  f False True  = 0.29
  f False False = 0.0

-- P(B=T | A=T)
query1 =
  let b' = burglary
      e' = earthquake
      a' = alarm b' e' =: True
  in  (b', True) `given` [a',e']

-- P(E=T | A=T)
query2 =
  let b' = burglary
      e' = earthquake
      a' = alarm b' e' =: True
  in (e', True) `given` [b',a']

data Person = Mary | John

calls :: Person -> Dist Bool -> Dist Bool
calls Mary vAlarm = vAlarm |> (bernoulli . f)
 where
  f True  = 0.9
  f False = 0.05
calls John vAlarm = vAlarm |> (bernoulli . f)
 where
  f True  = 0.9
  f False = 0.05

-- P (B=T | calls John = T, calls Mary = T)
query3 =
  let b' = burglary
      e' = earthquake
      a' = alarm b' e'
      c1 = calls John a' =: True
      c2 = calls Mary a' =: True
  in (b',True) `given` [e',a',c1,c2]

-- P (E=T | calls John = T, calls Mary = T)
query4 =
  let b' = burglary
      e' = earthquake
      a' = alarm b' e'
      c1 = calls John a' =: True
      c2 = calls Mary a' =: True
  in (e',True) `given` [a',c1,c2,b']