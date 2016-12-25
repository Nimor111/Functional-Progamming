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
fifteenLengthChain xs = length $ filter (\x -> length x > 15 ) $ map chain xs

-- map (*) [0..] returns [(0*), (1*), (2*)..] and we can do snazzy stuff
-- map (*) [0..] !! 4 $ 5 = 20 DAMN

main = do
  print $ filter (>2) [-1,0,1,2,3,4]
  print $ map (+1) [1,2,3,4,5]
  print $ sumOfOddSquares 10000
  print $ fifteenLengthChain [1..100]
