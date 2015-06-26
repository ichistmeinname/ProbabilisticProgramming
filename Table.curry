{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module Table where

data Table k v = Table [TableEntry k v]
 deriving (Eq,Ord)

instance (Show k, Show v) => Show (Table k v) where
  show (Table entries) = unlines (map show entries)

data TableEntry k v = TableEntry k v

instance (Show k, Show v) => Show (TableEntry k v) where
  show (TableEntry k v) = show k ++ "     " ++ show v

value :: TableEntry k v -> v
value (TableEntry _ val) = val

key :: TableEntry k v -> k
key (TableEntry k _) = k

instance (Eq a, Eq b) => Eq (TableEntry a b) where
  TableEntry k1 v1 == TableEntry k2 v2 = k1 == k2 && v1 == v2

instance (Ord a, Ord b) => Ord (TableEntry a b) where
  compare (TableEntry k1 v1) (TableEntry k2 v2)
    | k1 /= k2 = case v1 < v2 of
                      True  -> LT
                      False -> GT
  TableEntry k1 v1 < TableEntry k2 v2
    | k1 /= k2  = v1 < v2
    | otherwise = False
  TableEntry k1 v1 >= TableEntry k2 v2
    | k1 /= k2  = v1 >= v2
    | otherwise = False

weakerThan :: (Ord a, Ord b) => TableEntry a b
                             -> TableEntry a b
                             -> Bool
weakerThan = (<)

strongerThan :: (Ord a, Ord b) => TableEntry a b
                               -> TableEntry a b
                               -> Bool
strongerThan = (>=)

lookupTable :: Eq k => k -> Table k v -> Maybe v
lookupTable key (Table []) = Nothing
lookupTable key (Table (TableEntry k' val:tes))
  | key == k' = Just val
  | otherwise = lookupTable key (Table tes)

updateTable :: Eq a => a -> (b -> b) -> Table a b -> Table a b
updateTable _ _ (Table []) = Table []
updateTable key f (Table (e@(TableEntry k v):tes))
  | k == key  = Table (TableEntry k (f v) : tes)
  | otherwise = Table (e:ts')
 where
  Table ts' = updateTable key f (Table tes)

addPoints :: (Eq a, Num b) => a -> b -> Table a b -> Table a b
addPoints k v = updateTable k (+ v)