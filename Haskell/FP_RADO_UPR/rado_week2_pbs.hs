listToNumber :: [Int] -> Int
listToNumber xs = helper xs (length xs - 1)
  where helper :: [Int] -> Int -> Int
        helper [] _ = 0
        helper (x:xs) count = x * (10 ^ count) + helper xs (count - 1)

suffix :: Eq a => [a] -> [a] -> Bool
suffix ys xs = helper (reverse ys) (reverse xs)
  where helper :: Eq a => [a] -> [a] -> Bool
        helper [] _ = True
        helper (y:ys) (x:xs)
          | y /= x = False
          | otherwise = helper ys xs

occurrenceOfElement :: Eq a => a -> [a] -> Int 
occurrenceOfElement _ [] = 0
occurrenceOfElement e xs = sum $ map (\ l -> if l == e then 1 else 0) xs 

occurrences :: [Int] -> [Int] -> [Int]
occurrences xs ys = [occurrenceOfElement x ys | x <- xs]

removeAt :: Eq a => Int -> [a] -> [a]
removeAt 0 (x:xs) =  xs
removeAt y (x:xs) = x : removeAt (y - 1) xs
removeAt _ _ = error "Index out of bounds"
