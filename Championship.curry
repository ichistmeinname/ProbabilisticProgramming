{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module Championship where

import PFLP

import Prelude hiding ((>>=))
import Maybe (fromJust)
import List
import SetFunctions

data Team = Frankfurt | Gladbach | Dortmund | Paderborn | Leverkusen | Hannover
       | Hoffenheim | Muenchen | Hertha | Koeln | Freiburg | Mainz | Augsburg
       | Stuttgart | Bremen | HamburgerSV | Wolfsburg | Schalke
  deriving (Eq,Show,Ord)

data Match = Match Team Team Result
  deriving (Eq,Show,Ord)
data Result = AwayVictory | Draw | HomeVictory
  deriving (Bounded,Enum,Eq,Show,Ord)

possibleResults :: [Result]
possibleResults = [minBound .. maxBound]

-- data Score = Int ::: Int

weakerThan :: (Team,Int) -> (Team,Int) -> Bool
weakerThan (t1,n) (t2,m)
  | t1 /= t2  = n < m
  | otherwise = False

strongerThan :: (Team,Int) -> (Team,Int) -> Bool
strongerThan e1@(t1,_) e2@(t2,_)
  | t1 /= t2 = not (weakerThan e1 e2)
  | otherwise = False

points :: Result -> (Int,Int)
points HomeVictory = (3,0)
points Draw        = (1,1)
points AwayVictory = (0,3)

currentPoints :: Team -> Table -> Int
currentPoints = lookup_

matchPoints :: Match -> ((Team, Int),(Team, Int))
matchPoints (Match t1 t2 res) = ((t1,res1),(t2,res2))
 where
  (res1,res2) = points res

type Table = [TableEntry]
type TableEntry = (Team,Int)

mkTableEntry :: Team -> Table -> TableEntry
mkTableEntry team table = (team, currentPoints team table)

type Matchday = [MatchdayEntry]
type MatchdayEntry = (Team,Team)

maxPoints :: [Matchday] -> Int
maxPoints mds = fst (points (maxBound :: Result)) * length mds

lookup_ :: Eq a => a -> [(a,b)] -> b
lookup_ x = fromJust . lookup x

update :: Eq a => a -> (b -> b) -> [(a,b)] -> [(a,b)]
update _ _ [] = []
update key f ((k,v):kvMap)
  | k == key  = (k,f v) : kvMap
  | otherwise = (k,v) : update key f kvMap

addPoints :: Team -> Int -> Table -> Table
addPoints k v = update k (+ v)

recalculateTable :: Match -> Table -> Table
recalculateTable (Match team1 team2 res) table =
  addPoints team1 res1 (addPoints team2 res2 table)
 where
  (res1,res2) = points res

updateTable :: [Matchday] -> Table -> (Table,[Match])
updateTable mds table = (foldr recalculateTable table matches,matches)
 where
  matches = concatMap playMatchDay mds

match :: Team -> Team -> Match
match t1 t2 = Match t1 t2 _

playMatchDay :: Matchday -> [Match]
playMatchDay = map (uncurry match)

matchDay31 :: Matchday
matchDay31 =  [ (Schalke,Stuttgart), (Wolfsburg,Hannover), (Augsburg,Koeln)
              , (Hoffenheim,Dortmund), (Bremen,Frankfurt), (Freiburg,Paderborn)
              , (Leverkusen,Muenchen), (Mainz,HamburgerSV), (Hertha,Gladbach) ]

matchDay32 :: Matchday
matchDay32 = [ (HamburgerSV,Freiburg), (Muenchen,Augsburg), (Dortmund,Hertha)
             , (Gladbach,Leverkusen), (Hannover,Bremen), (Frankfurt,Hoffenheim)
             , (Stuttgart,Mainz), (Paderborn,Wolfsburg), (Koeln,Schalke) ]

matchDay33 :: Matchday
matchDay33 = [ (Schalke,Paderborn), (Leverkusen,Hoffenheim), (Stuttgart,HamburgerSV)
             , (Wolfsburg,Dortmund), (Mainz,Koeln), (Augsburg,Hannover)
             , (Hertha,Frankfurt), (Bremen,Gladbach), (Freiburg,Muenchen) ]

matchDay34 :: Matchday
matchDay34 = [ (Muenchen,Mainz), (Dortmund,Bremen), (Gladbach,Augsburg)
             , (Hoffenheim,Hertha), (Hannover,Freiburg), (Frankfurt,Leverkusen)
             , (HamburgerSV,Schalke), (Koeln,Wolfsburg), (Paderborn,Stuttgart) ]

upcomingMatchdays :: [Matchday]
upcomingMatchdays =
  [matchDay31,matchDay32,matchDay33,matchDay34]

currentTable :: Table
currentTable =
  [(Muenchen, 76), (Wolfsburg, 61), (Gladbach, 57), (Leverkusen, 55)
  ,(Schalke, 42), (Augsburg, 42), (Hoffenheim, 40), (Dortmund,39)
  ,(Bremen, 39), (Mainz, 37),(Frankfurt, 36), (Koeln, 35), (Hertha, 34)
  ,(Freiburg, 30), (Hannover, 29), (HamburgerSV,28), (Paderborn, 28)
  ,(Stuttgart, 27)]

uniformMatch :: Team -> Team -> Dist Match
uniformMatch t1 t2 = Match t1 t2 <$> uniform possibleResults

filterMatchdays :: [Team] -> [Matchday] -> [Matchday]
filterMatchdays teams matchDays =
    map (filter (\ (t1,t2) ->  any (`elem` teams)
                                   [t1,t2]))
        matchDays

relegationReduced :: Question Table
relegationReduced matchF team mds curTable =
  relegation matchF team mds' table'
 where
  (mds',table') = filterT (< pointsBound) mds curTable
  pointsBound = currentPoints team curTable + maxPoints mds

filterT :: (Int -> Bool)
        -> [Matchday]
        -> Table
        -> ([Matchday],Table)
filterT cond mds curTable = (matchdays,table)
 where
  table = filter (cond . snd) curTable
  teams = map fst table
  matchdays = filterMatchdays teams mds

relegation :: Question Table
relegation matchF team matchDays table =
  question (\t -> length (filter (`weakerThan` mkTableEntry team t) t) >= 2)
           matchF
           team
           matchDays
           table

winner :: Question Table
winner matchF team mds table =
  question (\t -> null (filter (`strongerThan` mkTableEntry team t) t))
           matchF
           team
           mds
           table

nthPlace :: Int -> Question Table
nthPlace place matchF team matchdays table =
  question (\t -> length (filter (`pred` mkTableEntry team t) t) `cond` place')
           matchF
           team
           matchdays
           table
 where
  (pred,cond,place') = mkTriple
  mkTriple :: (TableEntry -> TableEntry -> Bool, Int -> Int -> Bool, Int)
  mkTriple
    | place >= teams `div` 2 = (strongerThan,(<),place)
    | otherwise              = (weakerThan,(>=),teams - place)
  teams = length table

type DistF a b = a -> Dist b
type Question a = DistF (Team,Team) Match
                -> Team
                -> [Matchday]
                -> Table
                -> Dist a

question :: (Table -> Bool) -> Question Table
question cond matchF team mds table =
  filterDist cond newTable
 where
  results :: Dist [Match]
  results = traverse matchF (concat mds)
  newTable :: Dist Table
  newTable = foldr (recalculateTable) table <$> results

-- ---------------------------------------------------------
-- Small Examples
-- ---------------------------------------------------------

problem q = countDist (q (uncurry uniformMatch) HamburgerSV (take 2 upcomingMatchdays) currentTable)
problemSmall q = countDist (q (uncurry uniformMatch) HamburgerSV [day31] table30)

countDist q =
  foldValues (\(Dist _ p) (Dist x q) -> Dist x (p+q))
             (Dist [] 0.0)
             (set0 q)

day31 = [ -- (Schalke,Stuttgart), --(Wolfsburg,Hannover),
          (Freiburg,Paderborn),(Mainz,HamburgerSV)]
table30 = [ -- (Freiburg,30),(Hannover, 29),
           (HamburgerSV,28), (Paderborn, 28)]
           --(Stuttgart, 27)]

-- ---------------------------------------------------------
-- Tournament
-- ---------------------------------------------------------

tTable :: Table
tTable = zip tTeams (repeat 0)

tTeams = [HamburgerSV,Bremen,Wolfsburg,Gladbach]

tGames :: [(Team,Team)]
tGames = [(HamburgerSV,Bremen),(Mainz,Hannover),(HamburgerSV,Hannover),(Bremen,Mainz)]
-- tGames = [(t1,t2) | t1 <- tTeams, t2 <- tTeams, t1 /= t2]

tWinner :: Team -> Dist Table
tWinner t = countDist (winner (uncurry customMatch) t [tGames] tTable)

-- 46 %	24 % 30 %

customMatch t1 t2 = Match t1 t2 <$> scale (zip [HomeVictory,Draw,AwayVictory] (case (t1,t2) of
  (Gladbach,Wolfsburg) -> [9,4,17]
  (Wolfsburg,Gladbach) -> [17,4,9]
  (Mainz,Hannover)     -> [4,8,6]
  (Hannover,Mainz)     -> [6,8,4]
  (Bremen,_)           -> [11,10,13]
  (_,Bremen)           -> [13,10,11]
  _                    -> [145,82,79]))