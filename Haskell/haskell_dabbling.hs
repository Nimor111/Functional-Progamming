iSort :: [Int] -> [Int]
iSort = foldr ins []

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y:ys)
  | x <= y = x:(y:ys)
  | otherwise = y : ins x ys
