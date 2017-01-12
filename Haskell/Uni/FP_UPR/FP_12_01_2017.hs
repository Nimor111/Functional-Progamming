import Data.Maybe
import Data.List

data Tree a = Empty | Node a (Tree a) (Tree a)

main = do
  print $ findSumOfPaths exampleTree
  print $ quickSort [1,2,3,4,5]
  -- print $ quickSort $ getMaxCounts xs
  print $ getMaxCount (findNumOfOccurrences (head (tail xs)))
  print $ getMaxCounts xs
  print $ ssort xs
    where
      xs = ["abababa", "jdfeejjjjse", "hgff", "ac"]
 

exampleTree :: Tree Int
exampleTree = Node 1
              (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty))
              (Node 5 (Node 6 Empty Empty) (Node 7 Empty Empty))

findSumOfPaths :: Tree Int -> Int
findSumOfPaths Empty = 0
findSumOfPaths (Node x Empty Empty) = x
findSumOfPaths (Node x lt rt) = x + max (findSumOfPaths lt) (findSumOfPaths rt)

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = left ++ [mid] ++ right
  where
    left = quickSort [y | y <- xs, y > x]
    mid = x 
    right = quickSort [y | y <- xs, x > y]

findNumOfOccurrences :: String -> [(String, Int)]
findNumOfOccurrences xs = map (\x -> (x,length x)) ((group . sort) xs)

getMaxCount :: [(String, Int)] -> Int
getMaxCount xs = maximum $ map snd xs

getMaxCounts :: [String] -> [Int]
getMaxCounts [] = []
getMaxCounts (x:xs) = quickSort ([getMaxCount (findNumOfOccurrences x)] ++ getMaxCounts xs)

ssort :: [String] -> [String]
ssort [] = []
ssort xs = helper xs (getMaxCounts xs)
  where
    helper :: [String] -> [Int] -> [String]
    helper xs [] = [] 
    helper xs (n:ls) = [x | x <- xs, getMaxCount (findNumOfOccurrences x) == n] ++ helper xs ls
