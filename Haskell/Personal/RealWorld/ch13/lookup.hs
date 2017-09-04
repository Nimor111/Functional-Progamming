import qualified Data.Map as Map

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup key ((x, y) : xs)
  | key == x = Just y
  | otherwise = myLookup key xs

mapFromAl :: Ord a => [(a, b)] -> Map.Map a b
mapFromAl = Map.fromList

split :: Eq a => a -> [a] -> [[a]]
split _ [] = [[]]
split delim str =
  let (before, remainder) = span (/= delim) str
   in 
   before : case remainder of
              [] -> []
              x -> split delim (tail x)
