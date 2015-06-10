{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module Bundesliga where

import Table

import Maybe (fromJust)

data BundesligaTeam = Frankfurt | Gladbach | Dortmund | Paderborn | Leverkusen
                    | Hannover | Hoffenheim | Muenchen | Hertha | Koeln
                    | Freiburg | Mainz | Augsburg | Stuttgart | Bremen
                    | HamburgerSV | Wolfsburg | Schalke
  deriving (Eq,Show,Ord)

-- ---------------------------------------------------------
--  Model for a (bundesliga) match
-- ---------------------------------------------------------

-- Functor-like container
data Identity a = Identity a

unwrap :: Identity a -> a
unwrap (Identity v) = v

wrap :: a -> Identity a
wrap = Identity

fmap :: (a -> b) -> Identity a -> Identity b
fmap f = wrap . f . unwrap

type Simulation a b = BundesligaTeam -> BundesligaTeam -> a
type MatchSimulation a = Simulation a Match

type BundesligaTable = Table BundesligaTeam Int

data Match = Match BundesligaTeam BundesligaTeam Result
  deriving (Eq,Show,Ord)

data Result = AwayVictory | Draw | HomeVictory
  deriving (Bounded,Enum,Eq,Show,Ord)

possibleResults :: [Result]
possibleResults = [minBound .. maxBound]

points :: Result -> (Int,Int)
points HomeVictory = (3,0)
points Draw        = (1,1)
points AwayVictory = (0,3)

currentPoints :: BundesligaTeam -> BundesligaTable -> Int
currentPoints team = fromJust . lookupTable team

mkTableEntry :: BundesligaTeam
             -> BundesligaTable
             -> TableEntry BundesligaTeam Int
mkTableEntry team table = TableEntry team (currentPoints team table)

recalculateTable :: Match -> BundesligaTable -> BundesligaTable
recalculateTable (Match team1 team2 res) table =
  addPoints team1 res1 (addPoints team2 res2 table)
 where
  (res1,res2) = points res

data Matchday = Matchday [MatchdayEntry]
type MatchdayEntry = (BundesligaTeam,BundesligaTeam)

matchdayEntries :: Matchday -> [MatchdayEntry]
matchdayEntries (Matchday mes) = mes

playMatchDay :: MatchSimulation a -> Matchday -> [a]
playMatchDay play = map (uncurry play) . matchdayEntries

maxPoints :: [Matchday] -> Int
maxPoints mds = fst (points maxBound) * length mds

filterMatchdays :: [BundesligaTeam] -> [Matchday] -> [Matchday]
filterMatchdays teams =
  map (Matchday
      . filter (\ (t1,t2) ->  any (`elem` teams)
                                  [t1,t2])
      . matchdayEntries)


-- ---------------------------------------------------------
--  Examplary data (Season 14/15)
-- ---------------------------------------------------------

matchDay31 :: Matchday
matchDay31 = Matchday
  [ (Schalke,Stuttgart), (Wolfsburg,Hannover), (Augsburg,Koeln)
  , (Hoffenheim,Dortmund), (Bremen,Frankfurt), (Freiburg,Paderborn)
  , (Leverkusen,Muenchen), (Mainz,HamburgerSV), (Hertha,Gladbach) ]

matchDay32 :: Matchday
matchDay32 = Matchday
  [ (HamburgerSV,Freiburg), (Muenchen,Augsburg), (Dortmund,Hertha)
  , (Gladbach,Leverkusen), (Hannover,Bremen), (Frankfurt,Hoffenheim)
  , (Stuttgart,Mainz), (Paderborn,Wolfsburg), (Koeln,Schalke) ]

matchDay33 :: Matchday
matchDay33 = Matchday
  [ (Schalke,Paderborn), (Leverkusen,Hoffenheim), (Augsburg,Hannover)
  , (Wolfsburg,Dortmund), (Mainz,Koeln), (Stuttgart,HamburgerSV)
  , (Hertha,Frankfurt), (Bremen,Gladbach), (Freiburg,Muenchen) ]

matchDay34 :: Matchday
matchDay34 = Matchday
  [ (Muenchen,Mainz), (Dortmund,Bremen), (Gladbach,Augsburg)
  , (Hoffenheim,Hertha), (Hannover,Freiburg), (Frankfurt,Leverkusen)
  , (HamburgerSV,Schalke), (Koeln,Wolfsburg), (Paderborn,Stuttgart) ]

upcomingMatchdays :: [Matchday]
upcomingMatchdays =
  [matchDay31,matchDay32,matchDay33,matchDay34]

currentTable :: BundesligaTable
currentTable = Table $
  map (uncurry TableEntry)
      [(Muenchen, 76), (Wolfsburg, 61), (Gladbach, 57), (Leverkusen, 55)
      ,(Schalke, 42), (Augsburg, 42), (Hoffenheim, 40), (Dortmund,39)
      ,(Bremen, 39), (Mainz, 37),(Frankfurt, 36), (Koeln, 35), (Hertha, 34)
      ,(Freiburg, 30), (Hannover, 29), (HamburgerSV,28), (Paderborn, 28)
      ,(Stuttgart, 27)]