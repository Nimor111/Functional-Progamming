findNumOfOccurences :: [Integer] -> Integer -> Integer
findNumOfOccurences xs x = sum (map (\ y -> if y == x then 1 else 0) xs)

sumUnique :: [[Integer]] -> Integer
sumUnique xs = sum (map (sum . (\ x -> filter (\ y -> findNumOfOccurences x y == 1) x) xs))
