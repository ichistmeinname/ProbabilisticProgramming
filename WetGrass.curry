{-

    The same problem was modelled in other languages too

     Anglican: http://www.robots.ox.ac.uk/~fwood/anglican/examples/bayes_net/index.html
     HANSEI: http://okmij.org/ftp/kakuritu/paper_examples.ml

-}


module WetGrass where

import BayesianNetwork
import PFLP

rain :: Dist Bool
rain = bernoulli 0.2

sprinkler :: Dist Bool -> Dist Bool
sprinkler vRain = vRain |> (bernoulli . f)
 where
  f False = 0.4
  f True  = 0.01

grass :: Dist Bool -> Dist Bool -> Dist Bool
grass vSprinkler vRain = vSprinkler ||| vRain |> (bernoulli . uncurry f)
 where
  f False False = 0.0
  f False True  = 0.8
  f True  False = 0.9
  f True  True  = 0.99

-- -------------------------------------
--  Example 2
-- -------------------------------------

cloudy :: Dist Bool
cloudy = bernoulli 0.5

rain' :: Dist Bool -> Dist Bool
rain' vCloudy = vCloudy |> (bernoulli . f)
 where
  f True  = 0.8
  f False = 0.2

sprinkler' :: Dist Bool -> Dist Bool
sprinkler' vCloudy = vCloudy |> (bernoulli . f)
 where
  f True  = 0.1
  f False = 0.5

grass' :: Dist Bool -> Dist Bool -> Dist Bool
grass' vSprinkler vRain = vSprinkler ||| vRain |> (bernoulli . uncurry f)
 where
  f False False = 0.0
  f False True  = 0.9
  f True  False = 0.9
  f True  True  = 0.99

-- -------------------------------------
--  Queries
-- -------------------------------------

-- -----------------
--  Example 1
-- -----------------

-- P(R=T | G=T) ~ 35.77 %
rainWhenGrass =
  let r' = rain
      s' = sprinkler r'
      g' = grass s' r' =: True
  in (r', True) `given` [s', g']
-- rainWhenGrass r s g =
--   -- let r' = rain
--       -- s' = sprinkler r'
--       -- g' = grass s' r'
--   (r, True) `given` [g (s r) r =: True,s r]

wetGrass r s g bools = jointProbability (zipWith (=:) [r, s r, g (s r) r] bools)

-- -----------------
--  Example 2
-- -----------------

-- P(R | G) = 0.708
rainWhenGrass' =
  let c' = cloudy
      r' = rain' c'
      s' = sprinkler' c'
      g' = grass' s' r' =: True
  in (r', True) `given` [s', g']

-- P(S=T | G=T) = 0.430
sprinklerWhenGrass =
  let c' = cloudy
      r' = rain' c'
      s' = sprinkler' c'
      g' = grass' s' r' =: True
  in (s', True) `given` [r', g']