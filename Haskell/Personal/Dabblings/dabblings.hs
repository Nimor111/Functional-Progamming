import Data.List

listCountOfRepeating :: Eq a => [a] -> [Int]
listCountOfRepeating = map length . group

listCountOfRepeating' :: Eq a => [a] -> [Int]
listCountOfRepeating' [] = []
listCountOfRepeating' xs = helper 1 xs
  where
    helper :: Eq a => Int -> [a] -> [Int]
    helper _ [] = []
    helper count [x] = [count]
    helper count (x:xs)
      | x == head xs = helper (count + 1) xs
      | otherwise = count : helper 1 xs

-- I am not going to do this in practice
rle :: String -> String
rle [] = []
rle xs = helper 1 xs
  where
    helper :: Int -> String -> String
    helper _ [] = []
    helper 1 [x] = [x]
    helper count [x] = show count ++ [x]
    helper count (x:xs)
      | x == head xs = helper (count + 1) xs
      | count == 1 && x /= head xs = x : helper 1 xs
      | otherwise = show count ++ [x] ++ helper 1 xs 
