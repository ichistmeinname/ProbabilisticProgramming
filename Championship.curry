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
weakerThan (_,n) (_,m) = n <= m

strongerThan :: (Team,Int) -> (Team,Int) -> Bool
strongerThan e1 e2 = not (weakerThan e1 e2)

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

type Matchday = [MatchdayEntry]
type MatchdayEntry = (Team,Team)

without :: Table -> Team -> Table
without []         _ = []
without (e@(t,_):ts) team
  | t == team = ts
  | otherwise = e : without ts team

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
recalculateTable (Match team1 team2 result) table =
  addPoints team1 res1 (addPoints team2 res2 table)
 where
  (res1,res2) = points result

updateTable :: [Matchday] -> Table -> (Table,[Match])
updateTable mds table = (foldr recalculateTable table matches,matches)
 where
  matches = concatMap playMatchDay mds

sortTable :: Table -> Table
sortTable = sortBy strongerThan

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

result :: Team -> Team -> Dist Match
result t1 t2 = Match t1 t2 <$> uniform possibleResults

filterMatchdays :: [Team] -> [Matchday] -> [Matchday]
filterMatchdays teams matchDays =
    map (filter (\ (t1,t2) ->  any (`elem` teams)
                                   [t1,t2]))
        matchDays

relegationReduced :: Team
                  -> [Matchday]
                  -> Table
                  -> Dist (Table,[Match])
relegationReduced team mds curTable =
  relegation team matchDays table
 where
  matchDays = filterMatchdays teams mds
  table     = filter ((< pointsBound) . snd) curTable
  teams     = map fst table
  pointsBound = currentPoints team curTable + maxPoints mds

relegation :: Team
            -> [Matchday]
            -> Table
            -> Dist (Table,[Match])
relegation team matchDays table = liftA2l (,)
  (filterDist  (\t -> length (filter (\ (t1,p1) -> t1 /= team && p1 < teamPoints t) t)
                       >= 2)
               newTable)
  -- (filterDist (\ t -> length (dropWhile ((/= team) . fst)  t)
  --                           >= 3)
  --           (sortTable <$> newTable))
  results
 where
  teamPoints t = currentPoints team t
  results :: Dist [Match]
  results = traverse (uncurry result) (concat matchDays)
  newTable :: Dist Table
  newTable = foldr (recalculateTable) table <$> results
  liftA2l :: (a -> b -> c) -> Dist a -> Dist b -> Dist c
  liftA2l f (Dist v p) (Dist w _) = Dist (f v w) p

problem = countDist HamburgerSV (take 2 upcomingMatchdays) currentTable
problemSmall = countDist HamburgerSV [day31] table30

countDist team mds table =
  foldValues (\(Dist _ p) (Dist x q) -> Dist x (p+q))
             (Dist ([],[]) 0.0)
             (set3 relegationReduced team mds table)

day31 = [ -- (Schalke,Stuttgart), --(Wolfsburg,Hannover),
          (Freiburg,Paderborn),(Mainz,HamburgerSV)]
table30 = [ -- (Freiburg,30),(Hannover, 29),
           (HamburgerSV,28), (Paderborn, 28)]
           --(Stuttgart, 27)]