import SetFunctions (foldValues, mapValues, set0, set3, Values, select)
import ValueSequence (addVS, emptyVS,vsToList, ValueSequence)
import SearchTree (dfsStrategy, someSearchTree, searchTreeSize, SearchTree(..))

bools = foldr (:) [] (replicate 1000000 True)

and :: Bool
and = foldr (&&) True (replicate 10000000 True)

andV :: Bool
andV = foldValues (&&) True (set0 (foldr (?) True (replicate 10000000 True)))

one = fst (select set0VerySmall)
mapV = mapValues (|| False) set0VerySmall

mapNormal = map (|| False) (replicate 1000000 True)

andVSmall :: Bool
andVSmall = foldValues (&&) True set0VerySmall

set0VerySmall :: Values Bool
set0VerySmall = set0 (foldr1 (?) (replicate 4 True))

set0Small :: Values Bool
set0Small = set0 (foldr (?) True (replicate 100000 True))

set3Small :: Values Bool
set3Small = set3 foldr (?) True (replicate 100000 True)

valueSeqs :: ValueSequence Bool
valueSeqs = foldr addVS emptyVS (replicate 1000000 True)

seqToList :: [Bool]
seqToList = vsToList valueSeqs

seqToBool = foldr (&&) True seqToList

sTree = someSearchTree (foldr (?) True (replicate 1000 True))

foldTree :: (a -> b) -> (b -> b -> b) -> b -> SearchTree a -> b
foldTree _ _ e (Fail _)   = e
foldTree f g _ (Value x)  = f x
foldTree f g e (Or t1 t2) = g e (foldTree f g (foldTree f g e t1) t2)

strategy = dfsStrategy sTree

size = searchTreeSize sTree

andT = foldr (&&) True (vsToList strategy)

inefficient | gen = 1
 where
  gen = foldr (?) True (replicate 100000000 False)