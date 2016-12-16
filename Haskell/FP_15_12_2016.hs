import Data.List

main :: IO()
main = do
  --print(ints [1,2,2,3] [2,2,2,2])
  --print(longestSubsequence "abaabbbcd")
  --print(group' [1,1,1,2,2,2,1,1,1])
  --print(map' (\ x -> x + 2) [1,2,3])
  --print(map' (2-) [1,2,3])
  --print(filter' (\ y -> y == 2) [1,2,2,3])
  --print(composition (+2) (*2) 2)
  --print(composition2 (+2) (*2) 2)
  --print(composition3 (+2) (*2) 2)
  --print(composition4 (+2) (*2) 2)
  print (map ((*2) . (+1)) [1,2,3])
  print (partial2 (==) 2 3)
  
rem2 :: Eq a => a -> [a] -> [a]
rem2 _ [] = []
rem2 a (x:xs) = if a == x then xs else x : rem2 a xs

ints :: Eq a => [a] -> [a] -> [a]
ints [] _ = []
ints _ [] = []
ints (x:xs) ys
  | x `elem` ys = x : ints xs (x `rem2` ys)
  | otherwise = ints xs ys
  
longestSubsequence :: String -> String
longestSubsequence str = head [x | x <- group' str, length x == maximum groupedLst]
  where groupedLst = map length (group' str)
  
partial2 f = f 

group' :: Eq a => [a] -> [[a]]
group' []    = []
group' (x:xs) = (x : takeWhile (== x) xs) : group' (dropWhile (== x) xs)

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) = if f x then x : filter' f xs else filter' f xs

-- composition :: (b -> c) -> (a -> b) -> (a -> c)
--composition f g = \ x -> f (g x)

composition2 :: (b -> c) -> (a -> b) -> (a -> c)
composition2 f g x = f (g x)

composition3 ::  (b -> c) -> (a -> b) -> (a -> c)
composition3 f g = f . g

composition4 ::  (b -> c) -> (a -> b) -> (a -> c)
composition4 = (.)
