import Data.List

data Tree a = Empty | Node a (Tree a) (Tree a)

main = do
  -- print $ minCount exampleTree 1
  -- print $ longestIncreasingPrefix [1, 3, 2]
  -- print $ longestIncreasingPrefix [1, 2, 3]
  -- print $ longestIncreasingPrefix [1]
  -- print $ longestIncreasingPrefix [1, 2, 3, 4, 2]
  -- print $ reorderTuples [(1, 2), (2, 2), (1, 10), (10, 1)]
  -- print $ mergeAndSortDigits 11 111
  -- print $ mergeAndSortDigits 123 456
  -- print $ mergeAndSortDigits 456 123
  -- print $ balance 50 [45, 5, 100]
  -- print $ balance 3 [2, 10, 15]
  -- print $ balance 1 [5, 10, 15, 36]
  print $ repeat 3 " "
  print $ repeater "Quack" 5 "!"
    where 
      repeat = repeater "I love Haskell"

exampleTree :: Tree Int
exampleTree = Node 1
                (Node 2 
                (Node 3 Empty Empty)
                (Node 4 Empty Empty))
                Empty

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Node _ lt rt) = 1 + countNodes lt + countNodes rt

minCount :: Eq a => Tree a-> a -> Int
minCount Empty _ = 0
minCount (Node y lt rt) x
  | x == y = if countNodes lt < countNodes rt then countNodes lt + 1 else countNodes rt + 1
  | otherwise = minCount lt x + minCount rt x 

longestIncreasingPrefix :: Ord a => [a] -> [a]
longestIncreasingPrefix [] = []
longestIncreasingPrefix [x] = [x]
longestIncreasingPrefix (x:xs)
  | x >= head xs = [x]
  | otherwise = x : longestIncreasingPrefix xs

reorderTuples :: Ord a => [(a, a)] -> [(a, a)]
reorderTuples = map (\(x, y) -> if x < y then (y, x) else (x, y)) 

toDigits :: Int -> [Int]
toDigits 0 = []
toDigits n = nub (n `mod` 10 : toDigits (n `div` 10))

toNumber :: [Int] -> Int
toNumber = foldl (\acc x -> 10 * acc + x) 0

mergeAndSortDigits :: Int -> Int -> Int
mergeAndSortDigits x y 
  | sum (toDigits x) <= sum (toDigits y) =
    toNumber $ nub $ (reverse . toDigits) x ++ (reverse . toDigits) y
  | otherwise = toNumber $ nub (toDigits x ++ toDigits y)

balance :: Int -> [Int] -> Int
balance n xs = length xs - (length . takeOutEls n) xs 

takeOutEls :: Int -> [Int] -> [Int]
takeOutEls _ [] = []
takeOutEls n xs = helper 0 xs
  where
    helper :: Int -> [Int] -> [Int]
    helper _ [] = []
    helper y (x:xs)
      | y + x > n = []
      | otherwise = x : helper (y + x) xs

repeater :: String -> (Int -> String -> String)
repeater str = \x y -> tail (helper x y)
  where
    helper :: Int -> String -> String
    helper 0 _ = ""
    helper k s = s ++ str ++ helper (k - 1) s
