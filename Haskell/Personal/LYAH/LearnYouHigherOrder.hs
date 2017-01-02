multThree :: Num a => a -> a -> a -> a
multThree x y z = x * y * z

applyTwice :: ( a -> a ) -> a -> a
applyTwice f x = f $ f x

zipWith' :: ( a -> b -> c ) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: ( a -> b -> c ) -> b -> a -> c
flip' f x y = f y x

map' :: ( a -> b ) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

filter' :: ( a -> Bool ) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

sumOfOddSquares :: Int -> Int
sumOfOddSquares x = sum $ takeWhile (<x) (filter odd $ map (^2) [1..])

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x
  | even x = x : chain ( x `div` 2 )
  | odd x = x : chain ( x * 3 + 1 ) 

fifteenLengthChain :: (Integral a) => [a] -> Int
fifteenLengthChain xs = length $ filter (\xs -> length xs > 15 ) $ map chain xs

-- map (*) [0..] returns [(0*), (1*), (2*)..] and we can do snazzy stuff
-- map (*) [0..] !! 4 $ 5 = 20 DAMN

-- folds, fu syntastic i'm practising
-- sum' :: Num a => [a] -> a
-- sum' = foldl (+) 0

elem' :: Eq a => a -> [a] -> Bool
elem' y = foldl (\acc x -> ( x == y || acc )) False

-- note : this can be done with foldl but then we'd need ++ operator
-- which is more expensive than : so use foldr when building new lists
-- from a list
map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x acc -> f x : acc) []

-- note : right folds work on infinite lists, left folds don't!

maximum' :: Ord a => [a] -> a
maximum' = foldr1 (\ x acc -> if x > acc then x else acc)

maximum'' :: Ord a => [a] -> a
maximum'' = foldl1 (\acc x  -> if x > acc then x else acc)

-- the following two are equivalent, damn
-- reverse' :: [a] -> [a]
-- reverse' = foldl (\acc x -> x : acc) []

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

product' :: Num a => [a] -> a
product' = foldr1 (*)

filter'' :: (a -> Bool ) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\ _ x -> x)

-- map (\xs -> negate (sum ( tail xs))) [[1..5], [3..6], [1..7]]
func = map (negate . sum . tail)

main = do
  -- print $ filter (>2) [-1,0,1,2,3,4]
  -- print $ map (+1) [1,2,3,4,5]
  -- print $ sumOfOddSquares 10000
  -- print $ fifteenLengthChain [1..100]
  -- print $ elem' 5 [1,2,3,4,5]
  -- print $ maximum' [1,2,3,4]
  print $ maximum'' [1,2,3,4]
  print $ reverse' [1,2,3,4]
  print $ filter'' even [1,2,3,4]
  print $ head' [1,2,3,4]
  print $ last' [1,2,3,4]
