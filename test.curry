import SetFunctions (foldValues, set0, set3, Values)
import ValueSequence (addVS, emptyVS,vsToList, ValueSequence)
import SearchTree (dfsStrategy, someSearchTree, searchTreeSize)

bools = foldr (:) [] (replicate 1000000 True)

and :: Bool
and = foldr (&&) True (replicate 10000000 True)

andV :: Bool
andV = foldValues (&&) True (set0 (foldr (?) True (replicate 10000000 True)))

andVSmall :: Bool
andVSmall = foldValues (&&) True (set0 (foldr (?) True (replicate 100000 True)))

set0Small :: Values Bool
set0Small = set0 (foldr (?) True (replicate 100000 True))

set3Small :: Values Bool
set3Small = set3 foldr (?) True (replicate 100000 True)

valueSeqs :: ValueSequence Bool
valueSeqs = foldr addVS emptyVS (replicate 1000000 True)

seqToList :: [Bool]
seqToList = vsToList valueSeqs

seqToBool = foldr (&&) True seqToList

sTree = someSearchTree (foldr (?) True (replicate 1000000 True))

strategy = dfsStrategy sTree

size = searchTreeSize sTree

andT = foldr (&&) True (vsToList strategy)