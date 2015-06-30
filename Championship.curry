{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module Championship where

import Bundesliga
import qualified Table as T (Table(..), value, key, TableEntry(..),
                             strongerThan,weakerThan)
import PFLP

import Prelude hiding ((>>=))
import Maybe (fromJust)
import SetFunctions (set0, set4, foldValues, mapValues, Values)
import List (sum)

-- ---------------------------------------------------------
--  Match simulation
-- ---------------------------------------------------------

-- (pure) nondeterministic approach
match :: Simulation Match
match t1 t2 = Match t1 t2 _

-- probabilistic nondeterminstic approach
uniformMatch :: Simulation (Dist Match)
uniformMatch t1 t2 = Match t1 t2 <$> uniform possibleResults


-- ---------------------------------------------------------
--  Helper functions to reduce the search space
-- ---------------------------------------------------------

type Reduction = BundesligaTeam
               -> [Matchday]
               -> BundesligaTable
               -> ([Matchday],BundesligaTable)

relegationReduced :: Reduction
relegationReduced team mds curTable =
  filterT (< pointsBound) mds curTable
 where
  pointsBound = currentPoints team curTable + maxPoints mds

noReduction :: Reduction
noReduction = (\_ -> (,))

-- ---------------------------------------------------------
--  Questions
-- ---------------------------------------------------------

type AQuestion a b = Simulation a
                   -> BundesligaTeam
                   -> [Matchday]
                   -> BundesligaTable
                   -> b
type Question = AQuestion (Dist Match) (Dist BundesligaTable)

questionWithReduction :: Reduction -> (BundesligaTable -> Bool) -> Question
questionWithReduction reduceSS cond matchF team mds table =
  filterT cond newTable
 where
  (mds',table') = reduceSS team mds table
  results = traverse (uncurry matchF) (concatMap matchdayEntries mds')
  newTable = foldDist (recalculateTable) table' results
  -- traverse = map
  -- filterT p t | p t = t
  filterT = filterDist
  foldDist f e dist = foldr f e <$> dist


question :: (BundesligaTable -> Bool) -> Question
question = questionWithReduction noReduction

filterT :: (Int -> Bool)
        -> [Matchday]
        -> BundesligaTable
        -> ([Matchday],BundesligaTable)
filterT cond mds (T.Table curTable) = (matchdays,T.Table table)
 where
  table = filter (cond . T.value) curTable
  teams = map T.key table
  matchdays = filterMatchdays teams mds

relegation :: Question
relegation matchF team matchDays table =
  questionWithReduction relegationReduced
                     (\t@(T.Table tes) ->
                        length (filter (`T.weakerThan` mkTableEntry team t)
                                        tes)
                          >= 2)
                     matchF
                     team
                     matchDays
                     table

winner :: Question
winner matchF team mds table =
  question
                     (\t@(T.Table tes) ->
                        null (filter (`T.strongerThan` mkTableEntry team t)
                                     tes))
                     matchF
                     team
                     mds
                     table

-- at least nth place
nthPlace :: Int -> Question
nthPlace place matchF team matchdays table@(T.Table tEntries) =
  question
                     (\t@(T.Table tes) ->
                        length (filter (`pred` mkTableEntry team t)
                                       tes)
                         `cond` place')
                     matchF
                     team
                     matchdays
                     table
 where
  (pred,cond,place') = mkTriple
  mkTriple :: ( T.TableEntry BundesligaTeam Int
                 -> T.TableEntry BundesligaTeam Int
                 -> Bool
              , Int -> Int -> Bool
              , Int)
  mkTriple
    | place >= teams `div` 2 = (T.strongerThan,(<),place)
    | otherwise              = (T.weakerThan,(>=),teams - place)
  teams = length tEntries


-- ---------------------------------------------------------
--  Evaluation
-- ---------------------------------------------------------

------------------------------
--  Nondeterminstic Evaluation
-- ---------------------------
-- percentageForQuestion :: Reduction
--                       -> Question
--                       -> Simulation Match
--                       -> BundesligaTeam
--                       -> [Matchday]
--                       -> BundesligaTable
--                       -> (Float,Int,Int)
-- percentageForQuestion reduceSS q play team mds curTable =
--   ((fromInteger pos1 / fromInteger pos2) * 100,pos1,pos2)
--  where
--   pos1    = countValues (set4 q play team matches table)
--   pos2    = countOutcomes matches
--   (matches,table) = reduceSS team mds curTable

-- countValues :: Values a -> Int
-- countValues = foldValues (\_ n -> n + 1) 0 . mapValues (\_ -> 1)

-- countOutcomes :: [Matchday] -> Int
-- countOutcomes mds =
--   length possibleResults `pow` length (concatMap matchdayEntries mds)

-- pow :: Integral a => a -> a -> a
-- pow a b | b>= 0 = powaux 1 a b
--   where
--     powaux n x y = if y == 0 then ngg216
--                    else powaux (n * if (y `mod` 2 == 1) then x else 1)
--                                (x * x)
--                                (y `div` 2)

------------------------------
--  Probabilistic Evaluation
-- ---------------------------
countDist q =
  foldValues (\(Dist _ p) (Dist x q) -> Dist x (p+q))
             (Dist (T.Table []) 0.0)
             (set0 q)


-- ---------------------------------------------------------
-- Small Examples
-- ---------------------------------------------------------

-- Examples for Set-Function-Approach
-- problem q = q match
--               HamburgerSV
--               (take 2 upcomingMatchdays)
--               currentTable
-- problemSmall q = q match HamburgerSV [day31] table30

-- Examples for Probabilistic Approach
problem q = q uniformMatch
              HamburgerSV
              (take 3 upcomingMatchdays)
              currentTable
problemSmall q = q uniformMatch HamburgerSV [day31] table30


day31 = [ -- (Schalke,Stuttgart), --(Wolfsburg,Hannover),
          (Freiburg,Paderborn),(Mainz,HamburgerSV)]
table30 = [ -- (Freiburg,30),(Hannover, 29),
           (HamburgerSV,28), (Paderborn, 28)]
           --(Stuttgart, 27)]

-- ---------------------------------------------------------
-- Tournament
-- ---------------------------------------------------------


-- This is all we need for now:

-- data Dist a = Dist a Probability
-- data Probability = Prob Float

-- uniform :: [a] -> Dist a
-- uniform xs = Dist (foldr1 (?) xs) (Prob (1.0 / fromInteger count))
--  where
--   count = length xs

-- scale :: [(a,Float)] -> Dist a
-- scale xs = foldr (\(x,p) acc -> Dist x (Prob (p/q)) ? acc) failed xs
--  where
--   q = sum (map snd xs)

tTable :: BundesligaTable
tTable = T.Table (zipWith T.TableEntry tTeams (repeat 0))

tTeams :: [BundesligaTeam]
tTeams = [HamburgerSV,Bremen,Hannover,Mainz]

tGames :: Matchday
tGames = Matchday
  [(HamburgerSV,Bremen),(Mainz,Hannover)
  ,(HamburgerSV,Hannover),(Bremen,Mainz)]

tWinner :: BundesligaTeam -> Dist BundesligaTable
tWinner t = winner uniformMatch t [tGames] tTable

customMatch t1 t2 = uniform [match HomeVictory, match Draw, match AwayVictory]
 where
  match res = Match t1 t2 res
  -- scale (zip [HomeVictory,Draw,AwayVictory] (case (t1,t2) of
  -- (Gladbach,Wolfsburg) -> [9,4,17]
  -- (Wolfsburg,Gladbach) -> [17,4,9]
  -- (Mainz,Hannover)     -> [4,8,6]
  -- (Hannover,Mainz)     -> [6,8,4]
  -- (Bremen,_)           -> [11,10,13]
  -- (_,Bremen)           -> [13,10,11]
  -- _                    -> [145,82,79]))