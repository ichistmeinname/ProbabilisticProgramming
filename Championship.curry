{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module Championship where

import Bundesliga
import qualified Table as T (Table(..), value, key, TableEntry(..),
                             strongerThan,weakerThan)
import PFLP

import Prelude hiding ((>>=))
import FiniteMap
import Maybe (fromJust)
-- import List
import SetFunctions (set0, set4, foldValues, mapValues, Values)

-- ---------------------------------------------------------
--  Match simulation
-- ---------------------------------------------------------

-- (pure) nondeterministic approach
match :: BundesligaTeam -> BundesligaTeam -> Match
match t1 t2 = Match t1 t2 _

-- probabilistic nondeterminstic approach
uniformMatch :: BundesligaTeam -> BundesligaTeam -> Dist Match
uniformMatch t1 t2 = Match t1 t2 <$> uniform possibleResults


-- ---------------------------------------------------------
--  Filter to reduce search space
-- ---------------------------------------------------------

type Filter = BundesligaTeam
            -> [Matchday]
            -> BundesligaTable
            -> ([Matchday],BundesligaTable)

relegationReduced :: Filter
relegationReduced team mds curTable =
  filterT (< pointsBound) mds curTable
 where
  pointsBound = currentPoints team curTable + maxPoints mds

noFilter :: Filter
noFilter = (\_ -> (,))

-- ---------------------------------------------------------
--  Questions
-- ---------------------------------------------------------

type AQuestion a b = (MatchdayEntry -> a)
                   -> BundesligaTeam
                   -> [Matchday]
                   -> BundesligaTable
                   -> b
type Question = AQuestion Match BundesligaTable

questionWithFilter :: Filter -> (BundesligaTable -> Bool) -> Question
questionWithFilter reduceSS cond matchF team mds table =
  filterT cond newTable
 where
  (mds',table') = reduceSS team mds table
  results = traverse matchF (concatMap matchdayEntries mds')
  newTable = foldr (recalculateTable) table' results
  traverse = map
  filterT p t | p t = t
  -- filterT = filterDist
  -- foldDist f e dist = foldr f e <$> dist


question :: (BundesligaTable -> Bool) -> Question
question = questionWithFilter noFilter

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
  questionWithFilter relegationReduced
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
percentageForQuestion :: Filter
                      -> Question
                      -> (MatchdayEntry -> Match)
                      -> BundesligaTeam
                      -> [Matchday]
                      -> BundesligaTable
                      -> (Float,Int,Int)
percentageForQuestion reduceSS q play team mds curTable =
  ((fromInteger pos1 / fromInteger pos2) * 100,pos1,pos2)
 where
  pos1    = countValues (set4 q play team matches table)
  pos2    = countOutcomes matches
  (matches,table) = reduceSS team mds curTable

countValues :: Values a -> Int
countValues = foldValues (\_ n -> n + 1) 0 . mapValues (\_ -> 1)

countOutcomes :: [Matchday] -> Int
countOutcomes mds =
  length possibleResults `pow` length (concatMap matchdayEntries mds)

pow :: Integral a => a -> a -> a
pow a b | b>= 0 = powaux 1 a b
  where
    powaux n x y = if y == 0 then n
                   else powaux (n * if (y `mod` 2 == 1) then x else 1)
                               (x * x)
                               (y `div` 2)

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

problem q = q (uncurry match)
              HamburgerSV
              (take 2 upcomingMatchdays)
              currentTable
problemSmall q = q (uncurry match) HamburgerSV [day31] table30


day31 = [ -- (Schalke,Stuttgart), --(Wolfsburg,Hannover),
          (Freiburg,Paderborn),(Mainz,HamburgerSV)]
table30 = [ -- (Freiburg,30),(Hannover, 29),
           (HamburgerSV,28), (Paderborn, 28)]
           --(Stuttgart, 27)]

-- ---------------------------------------------------------
-- Tournament
-- ---------------------------------------------------------

tTable :: BundesligaTable
tTable = T.Table (zipWith T.TableEntry tTeams (repeat 0))

tTeams = [HamburgerSV,Bremen,Hannover,Mainz]

tGames :: Matchday
tGames = Matchday
  [(HamburgerSV,Bremen),(Mainz,Hannover)
  ,(HamburgerSV,Hannover),(Bremen,Mainz)]
-- tGames = Matchday [(t1,t2) | t1 <- tTeams, t2 <- tTeams, t1 /= t2]

-- tWinner :: BundesligaTeam -> Dist BundesligaTable
tWinner t = winner (uncurry match) t [tGames] tTable

-- 46 % 24 % 30 %

customMatch t1 t2 = Match t1 t2 <$> uniform possibleResults
  -- scale (zip [HomeVictory,Draw,AwayVictory] (case (t1,t2) of
  -- (Gladbach,Wolfsburg) -> [9,4,17]
  -- (Wolfsburg,Gladbach) -> [17,4,9]
  -- (Mainz,Hannover)     -> [4,8,6]
  -- (Hannover,Mainz)     -> [6,8,4]
  -- (Bremen,_)           -> [11,10,13]
  -- (_,Bremen)           -> [13,10,11]
  -- _                    -> [145,82,79]))