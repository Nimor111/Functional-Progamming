maximum' :: Ord a => [a] -> a
maximum' [] = error "Empty list"
maximum' [x] = x
maximum' (x:xs) = if x > maximum' xs then x else maximum' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

cycle' :: Num a => [a] -> [a]
cycle' xs = xs ++ cycle' xs

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs) = if y == x then True else elem' y xs

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = smaller ++ [x] ++ bigger
  where smaller = quickSort [a | a <- xs, a <= x]
        bigger = quickSort [a | a <- xs, a > x]

main = do
  print(maximum' [1,2,3,4])
  print(replicate' 3 5)
  print(take' 0 [1,2,3,4])
  print(take' 6 (cycle' [1,3,3]))
  print(zip' [1,2,3] [4,5,6,7])
  print(elem' 6 [1,2,3,4,5])
  print(quickSort [1,5,2,1,0,9])