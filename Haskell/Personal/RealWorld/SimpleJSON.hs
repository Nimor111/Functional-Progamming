sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

fn :: [Int] -> [Int]
fn = map (+1)
