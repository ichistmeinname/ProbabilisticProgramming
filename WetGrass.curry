{-

    The same problem was modelled in other languages too

     Anglican: http://www.robots.ox.ac.uk/~fwood/anglican/examples/bayes_net/index.html
     HANSEI: http://okmij.org/ftp/kakuritu/paper_examples.ml

-}


module WetGrass where

import Prelude hiding ((>>=))
import BayesianNetwork
import PFLP

-- -------------------------------------
--  Example 1
-- -------------------------------------

-- data Dist a      = Dist a Probability
-- type Probability = Float

rain :: Dist Bool
rain = bernoulli 0.2

sprinkler :: Bool -> Dist Bool
sprinkler False = bernoulli 0.4
sprinkler True  = bernoulli 0.01

grassWet :: Bool -> Bool -> Dist Bool
grassWet False False = bernoulli 0.0
grassWet False True  = bernoulli 0.8
grassWet True  False = bernoulli 0.9
grassWet True  True  = bernoulli 0.99

grassWetWhenRain =
  let r' = rain =: True
      s' = sprinkler =<< r' =: False
      g = s' >>= \s ->
          r' >>= \r ->
            grassWet s r
  in g =: True

-- P(R=T | G=T) ~ 35.77 %
rainWhenGrass =
  let r' = rain
      s' = sprinkler =<< r'
      g' = s' >>= \s ->
           r' >>= \r ->
             grassWet s r
  in (r', True) `given` [s', g' =: True]

grassWetWhenRain' = rain >>= \rS ->
                    sprinkler rS >>= \s ->
                    rain >>= \r ->
                      grassWet s r =: True

-- -------------------------------------
--  Example 2 (with different notation)
-- -------------------------------------

infixl 4 |>
infixl 5 |||

(|>) :: Dist a -> (a -> Dist b) -> Dist b
Dist vA _ |> f = f vA

(|||) :: Dist a -> Dist b -> Dist (a,b)
(|||) = liftA2 (,)

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