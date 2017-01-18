import Data.List

data Order = Online Float Int Int | Offline Float
data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

main = do 
  print $ levelSum 3 exampleTree
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
  -- print $ repeat 3 " "
  -- print $ repeater "Quack" 5 "!"
  --   where 
  --     repeat = repeater "I love Haskell"
  -- print $ isOnline (Online 1 2 3)
  -- print $ timeUntilReceiving (Online 1.5 2 3)
  -- print $ totalPrice [Online 2.5 1 1, Online 3 1 2, Offline 5.5]
  -- print $ onlineOrders [Online 2.5 1 1, Online 3 1 2, Offline 5.5]
  -- print $ isExpensive (Online 110.5 2 3)
  -- print $ isExpensive (Offline 99.9)
  -- print (Online 5.5 2 3)
  -- print (Offline 4.53)
  -- print $ Online 5 4 3 == Online 5 4 3
  print $ levelSum 2 exampleTree

exampleTree :: Tree Int
exampleTree = Node 1
                (Node 2 
                (Node 3 Empty Empty)
                (Node 4 Empty Empty))
                (Node 6 Empty Empty)

-- Problem 7 - Tree data type
levelSum :: Int -> Tree Int -> Int
levelSum _ Empty = 0
levelSum 1 (Node e _ _) = e 
levelSum k (Node e lt rt) = levelSum (k - 1) lt + levelSum (k - 1) rt

-- levelSum :: Int -> Tree Int -> Int
-- levelSum _ Empty = 0
-- levelSum k tree = (sum . treeLevel k) tree

-- Problem 6 - Order data type
isOnline :: Order -> Bool
isOnline Online{} = True
isOnline _ = False

timeUntilReceiving :: Order -> Int
timeUntilReceiving (Offline _) = 0
timeUntilReceiving (Online _ _ h) = h

totalPrice :: [Order] -> Float
totalPrice [] = 0
totalPrice (Online p _ _:xs) = p + totalPrice xs
totalPrice (Offline p:xs) = p + totalPrice xs

onlineOrders :: [Order] -> Int
onlineOrders [] = 0
onlineOrders (Offline _:xs) = onlineOrders xs
onlineOrders (Online{}:xs) = 1 + onlineOrders xs

isExpensive :: Order -> Bool
isExpensive (Offline p) = p > 100
isExpensive (Online p _ _) = p > 100

instance Show Order where
  show (Online p n h) = show p ++ " " ++ show n ++ " " ++ show h
  show (Offline p) = show p

instance Eq Order where
  (Online p n h) == (Online p1 n1 h1) = p == p1 && n == n1 && h == h1
  (Offline p) == (Offline p1) = p == p1

-- Problem 1 - Longest increasing prefix
longestIncreasingPrefix :: Ord a => [a] -> [a]
longestIncreasingPrefix [] = []
longestIncreasingPrefix [x] = [x]
longestIncreasingPrefix (x:xs)
  | x >= head xs = [x]
  | otherwise = x : longestIncreasingPrefix xs

-- Problem 2 - Reorder tuples
reorderTuples :: Ord a => [(a, a)] -> [(a, a)]
reorderTuples = map (\(x, y) -> if x < y then (y, x) else (x, y)) 

-- Problem 3 - Merge and sort digits
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

-- Problem 4 - Balance
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

-- Problem 5 - Repeater
repeater :: String -> (Int -> String -> String)
repeater str = \x y -> tail (helper x y)
  where
    helper :: Int -> String -> String
    helper 0 _ = ""
    helper k s = s ++ str ++ helper (k - 1) s
