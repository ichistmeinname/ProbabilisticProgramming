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
earthquake = bernoulli 0.2

burglary :: Dist Bool
burglary = bernoulli 0.7

alarm :: Dist Bool -> Dist Bool -> Dist Bool
alarm vBurglary vEarthquake =
  vBurglary |> \b ->
    vEarthquake |> \e -> bernoulli $ f b e
 where
  f True  True  = 0.9
  f True  False = 0.8
  f False True  = 0.1
  f False False = 0.0


-- P(B=T | A=T) = { True 98.97%, False 1.03% }
query1 =
  let b' = burglary
      e' = earthquake
      a' = alarm b' e' =: True
  in  (b', True) `given` [a',e']

-- P(E=T | A=T) = { False 77.24%, True 22.76% }
query2 =
  let b' = burglary
      e' = earthquake
      a' = alarm b' e' =: True
  in (e', True) `given` [b',a']

-- -----------------
--  Second version
-- -----------------

alarm' :: Bool -> Bool -> Dist Bool
alarm' True  True  = bernoulli 0.9
alarm' True  False = bernoulli 0.8
alarm' False True  = bernoulli 0.1
alarm' False False = bernoulli 0.0

query1' =
  burglary >>>= \b ->
  earthquake >>>= \e ->
  (alarm' b e =: True) >>>= \_ ->
  pure b
  
query2' =
  burglary >>>= \b ->
  earthquake >>>= \e ->
  alarm' b e >>>= \a ->
  guard a >>>= \_ ->
  pure e

-- -----------------
--  Extension
-- -----------------

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

-- P (B=T | calls John = T, calls Mary = T) = { True 98.19%, False 1.81% }
query3 =
  let b' = burglary
      e' = earthquake
      a' = alarm b' e'
      c1 = calls John a' =: True
      c2 = calls Mary a' =: True
  in (b',True) `given` [e',a',c1,c2]

-- P (E=T | calls John = T, calls Mary = T) = { False 77.31%, True 22.69% }
query4 =
  let b' = burglary
      e' = earthquake
      a' = alarm b' e'
      c1 = calls John a' =: True
      c2 = calls Mary a' =: True
  in (e',True) `given` [a',c1,c2,b']

-- -----------------
--  Second version
-- -----------------

calls' :: Person -> Bool -> Dist Bool
calls' _ True = bernoulli 0.8
calls' _ False = bernoulli 0.1

query3' =
  burglary >>>= \b ->
  earthquake >>>= \e ->
  alarm' b e >>>= \a ->
  calls' John a >>>= \c1 ->
  calls' Mary a >>>= \c2 ->
  guard c1 >>>= \_ ->
  guard c2 >>>= \_ ->
  pure b

query4' =
  burglary >>>= \b ->
  earthquake >>>= \e ->
  alarm' b e >>>= \a ->
  calls' John a >>>= \c1 ->
  calls' Mary a >>>= \c2 ->
  guard (c1 && c2) >>>= \_ ->
  pure e