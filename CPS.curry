module CPS where

import Float
import FiniteMap
import SetFunctions

type Prob = Float

data VC a = V a | C (() -> PV a)
type PV a = (Prob, VC a)

pvUnit :: a -> PV a
pvUnit x = (1.0, V x)

pvBind :: PV a -> (a -> PV b) -> PV b
pvBind (p, V x) f = (p, C (\ () -> f x))
pvBind (p, C t) f = (p, C (\ () -> pvBind (t ()) f))

type PM a = (a -> PV Bool) -> PV Bool
type Arr a b = a -> (b -> PV Bool) -> PV Bool

b :: Bool -> PM Bool
b x = \k -> k x

dist :: [(Prob,a)] -> PM a
dist ch = \k -> foldr1 (?) (map (\ (p,v) -> (p, C (\ () -> k v))) ch)

{-
flip 0.5
=
dist [(0.5,True), (1 -. 0.5, False)]
=
dist [(0.5,True), (0.5, False)]
=
\k -> foldr1 (?) (map (\ (p,v) (p, C (\ () -> k v))) [(0.5,True), (0.5, False)])
=
\k -> foldr1 (?) [(0.5, C (\ () -> k True)),(0.5, C (\ () -> k False))]
=
\k -> (0.5, C (\ () -> k True)) ? (0.5, C (\ () -> k False))


reify0 (flip 0.5)
=
flip 0.5 pvUnit
=
(\k -> (0.5, C (\ () -> k True)) ? (0.5, C (\ () -> k False))) pvUnit
=
(0.5, C (\ () -> pvUnit True)) ? (0.5, C (\ () -> pvUnit False))

-}

neg :: PM Bool -> PM Bool
neg e = \k -> e (\v -> k (not v))

con :: PM Bool -> PM Bool -> PM Bool
con e1 e2 = \k -> e1 (\v1 -> if v1 then e2 k else b False k)

dis :: PM Bool -> PM Bool -> PM Bool
dis e1 e2 = \k -> e1 (\v1 -> if v1 then b True k else e2 k)

if_ :: PM Bool -> (() -> PM a) -> (() -> PM a) -> PM a
if_ et e1 e2 = \k -> et (\t -> if t then e1 () k else e2 () k)

lam :: (PM a -> PM b) -> PM (Arr a b)
lam e = \k -> k (\x -> e (\k2 -> k2 x))

app :: PM (Arr a b) -> PM a -> PM b
app e1 e2 = \k -> e1 (\f -> e2 (\x -> f x k))

reify0 :: PM Bool -> PV Bool
reify0 m = m pvUnit


-----------------
-- Exploration --
-----------------

-- explore :: Maybe Int -> PV a -> PV a
-- explore maxdepth cs =
--   let loop :: Float -> Int -> Bool -> PV a -> (FM a Float, PV a) -> (FM a Float, PV a)
--       loop p depth down choices answers@(ans, susp) =
--         case choices of
--           [] -> answers
--           (pt, V v) : rest -> loop p depth down rest (addToFM_C (+.) ans v (pt *. p), susp)
--           (pt, C t) : rest | down ->
--             let down' = maybe True (depth <) maxdepth
--             in loop p depth down rest (loop (pt *. p) (depth + 1) down' (t ()) answers)
--           (pt, c) : rest -> loop p depth down rest (ans, (pt *. p, c) ? susp)
--       (ans1,susp1) = loop 1.0 0 True cs (emptyFM (<), [])
--    in foldFM (\v p a -> (p, V v) ? a) susp1 ans1

explore :: Maybe Int -> [PV a] -> IO [PV a]
explore maxdepth cs =
  let loop :: Float -> Int -> Bool -> [PV a] -> (FM a Float, [PV a]) -> IO (FM a Float, [PV a])
      loop p depth down choices answers@(ans, susp) =
        case choices of
          [] -> return answers
          (pt, V v) : rest -> loop p depth down rest (addToFM_C (+.) ans v (pt *. p), susp)
          (pt, C t) : rest | down ->
            let down' = maybe True (depth <) maxdepth
            in values2list (set1 t ()) >>= \cs' -> loop (pt *. p) (depth + 1) down' cs' answers >>= loop p depth down rest
          (pt, c) : rest -> loop p depth down rest (ans, (pt *. p, c) : susp)
  in loop 1.0 0 True cs (emptyFM (<), []) >>= \ (ans1,susp1) -> return (foldFM (\v p a -> (p, V v) : a) susp1 ans1)

{-

values2list (set1 reify0 (flip 0.5)) >>= explore (Just 1)
=
explore (Just 1) [(0.5, C (\ () -> pvUnit True)),(0.5, C (\ () -> pvUnit False))]
=
loop 1.0 0 True [(0.5, C (\ () -> pvUnit True)),(0.5, C (\ () -> pvUnit False))] (emptyFM (<), [])
 >>= \ (ans1,susp1) -> return (foldFM (\v p a -> (p, V v) : a) susp1 ans1)
=
(values2list (set1 (\ () -> pvUnit True) ()) >>= \cs' -> loop (0.5 * 1.0) (0+1) (0 < 1) cs' (emptyFM (<), [])
 >>= loop 1.0 0 True [(0.5, C (\ () -> pvUnit False))])
>>= \ (ans1,susp1) -> return (foldFM (\v p a -> (p, V v) : a) susp1 ans1)
=
(loop 0.5 1 True [(1.0, V True)] (emptyFM (<), [])
 >>= loop 1.0 0 True [(0.5, C (\ () -> pvUnit False))])
>>= \ (ans1,susp1) -> return (foldFM (\v p a -> (p, V v) : a) susp1 ans1)
=
(loop 0.5 1 True [] (addToFM_C (+.) (emptyFM (<)) 1.0 0.5, [])
  >>= loop 1.0 0 True [(0.5, C (\ () -> pvUnit False))])
>>= \ (ans1,susp1) -> return (foldFM (\v p a -> (p, V v) : a) susp1 ans1)
=
(loop 0.5 1 True [] ({True -> 0.5}, [])
  >>= loop 1.0 0 True [(0.5, C (\ () -> pvUnit False))])
>>= \ (ans1,susp1) -> return (foldFM (\v p a -> (p, V v) : a) susp1 ans1)
=
loop 1.0 0 True [(0.5, C (\ () -> pvUnit False))] ({True -> 0.5}, [])
  >>= \ (ans1,susp1) -> return (foldFM (\v p a -> (p, V v) : a) susp1 ans1)
=
(values2list (set1 (\ () -> pvUnit False) ()) >>= \cs' -> loop (0.5 *. 1.0) (0 + 1) (0 < 1) cs' ({True -> 0.5}, [])
 >>= loop 1.0 0 True [])
  >>= \ (ans1,susp1) -> return (foldFM (\v p a -> (p, V v) : a) susp1 ans1)
=
(loop 0.5 1 True [(1.0,V False)] ({True -> 0.5}, [])
 >>= loop 1.0 0 True [])
  >>= \ (ans1,susp1) -> return (foldFM (\v p a -> (p, V v) : a) susp1 ans1)
=
(loop 0.5 1 True [] ({False -> 0.5,True -> 0.5}, [])
 >>= loop 1.0 0 True [])
  >>= \ (ans1,susp1) -> return (foldFM (\v p a -> (p, V v) : a) susp1 ans1)
=
loop 1.0 0 True [] ({False -> 0.5,True -> 0.5}, [])
  >>= \ (ans1,susp1) -> return (foldFM (\v p a -> (p, V v) : a) susp1 ans1)
=
return (foldFM (\v p a -> (p, V v) : a) [] {False -> 0.5, True -> 0.5})
=
[(0.5, V False), (0.5, V True)]

-}

-----------------
--  LAWN Model --
-----------------

flip :: Float -> PM Bool
flip p = dist [(p,True), (1 -. p, False)]

let_ :: PM a -> (PM a -> PM b) -> PM b
let_ e f = app (lam f) e

grassModel :: PM Bool
grassModel =
  let_ (flip 0.3) (\rain ->
  let_ (flip 0.5) (\sprinkler ->
  let_ (dis (con (flip 0.9) rain)
            (dis (con (flip 0.8) sprinkler)
                 (flip 0.1))) (\grassIsWet ->
                 if_ grassIsWet (const rain) (const (dist [])))))

scp1 :: PV Bool
scp1 = reify0 (grassModel)

scpe1 :: IO [PV Bool]
scpe1 = values2list (set1 reify0 grassModel) >>= explore (Just 1)

scpe4 :: IO [PV Bool]
scpe4 = values2list (set1 reify0 grassModel) >>= explore (Just 4)

-- uniform :: [a] -> PM a
-- uniform xs = uniformInterval (foldr1 (?) xs) count
--  where
--   count = length xs

-- uniformInterval :: a -> Int -> PM a
-- uniformInterval val p = dist [(1.0 /. (i2f p), val)]

-- birthday :: Int -> PM Int
-- birthday = \_ -> uniform [1..366]

-- pairEqual :: Int -> Int -> Int -> PM Bool
-- pairEqual max p1 p2 =
--   let_ (birthday p1) (\b1 ->
--   let_ (birthday p2) (\b2 ->
--     if p1 > max
--       then b False
--       else if p2 > max
--              then pairEqual max (p1 + 1) (p1 + 2)
--              else if (b1 <==> b2)
--                      then b True
--                      else pairEqual max p1 (p2 + 1)))

-- (<==>) :: PM a -> PM a -> Bool
-- x <==> y = snd (reify0 x) == snd (reify0 y)