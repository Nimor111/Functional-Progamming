iSort :: [Int] -> [Int]
iSort = foldr ins []

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y:ys)
  | x <= y = x:(y:ys)
  | otherwise = y : ins x ys


-- not optimal solution, takes head of list to be partition
qSort :: [Int] -> [Int]
qSort [] = []
qSort (x:xs) = qSort [ y | y <- xs, y <= x] ++ [x]
            ++ qSort [ y | y <- xs, y > x]
