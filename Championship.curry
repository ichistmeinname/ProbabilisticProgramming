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
weakerThan (_,n) (_,m) = n < m

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
sortTable = sortBy (\ (_,p1)(_,p2) -> p1 <= p2)

match :: Team -> Team -> Match
match t1 t2 = Match t1 t2 _

playMatchDay :: Matchday -> [Match]
playMatchDay = map (uncurry match)

day31 = [ (Schalke,Stuttgart), (Wolfsburg,Hannover),
          (Freiburg,Paderborn),(Mainz,HamburgerSV)]
table30 = [(Freiburg,30),(Hannover, 29),
           (HamburgerSV,28), (Paderborn, 28),(Stuttgart, 27)]

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

func :: (a -> Dist b) -> [a] -> Dist [b]
func f []     = certainly []
func f (v:vs) = f v >>= \_ -> func f vs

func2 :: [Dist a] -> Dist [a]
func2 = foldr (\ (Dist v p) (Dist vs q) -> Dist (v:vs) (p*q)) (certainly [])

test :: Dist [Match]
test = traverse (uncurry result)
                [(HamburgerSV,Bremen),(HamburgerSV,Schalke)]

test1 :: [Dist Match]
test1 = map (uncurry result)
            [(HamburgerSV,Bremen),(HamburgerSV,Schalke)]

-- test1 :: Dist [Match]
-- test1 = func (\g -> mapDist (\p -> uncurry Match g p) gameProb)
--              [(HamburgerSV,Bremen),(HamburgerSV,Schalke)]

relegation :: Team
            -> [Matchday]
            -> Table
            -> Dist Table
relegation team matchDays table =
  filterDist (\ t -> length (dropWhile ((/= team) . fst)  (sortTable t))
                            >= 3)
            tmp
  -- r
  --   |> not (isEmpty (thereExistN 2 (newTable `without` team)
  --      `suchThat` anySet (all (`weakerThan` teamEntry))))
  --      -- `suchThatSet` all (`weakerThan` teamEntry)))
 where
  results :: Dist [Match]
  results = traverse (uncurry result) (concat matchDays)
  tmp :: Dist Table
  tmp = foldr (recalculateTable) table <$> results
  -- Dist a -> Dist b -> Dist c

problem = foldValues (\(Dist _ p) (Dist x q) -> Dist x (p+q)) (certainly failed) (set3 relegation HamburgerSV (take 2 upcomingMatchdays) currentTable)