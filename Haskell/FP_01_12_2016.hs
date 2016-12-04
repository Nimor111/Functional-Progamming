main :: IO ()
main = do
  print(fib 2)
  print(primeInterval 2 10)

-- Fibonacci recursively
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

fib2 :: Integer -> Integer
fib2 n = helper 1 1 1
  where helper :: Integer -> Integer -> Integer -> Integer
        helper a b count = if count < (n - 1)
            then helper b (a + b) (count + 1)
            else b

digits :: Integer -> Integer
digits n = helper n 0
  where
    helper :: Integer -> Integer -> Integer
    helper n sum = if n < 10
        then sum + n
        else helper (n `div` 10) (sum + n `mod` 10)

numDigs :: Integer -> Integer
numDigs 0 = 0
numDigs n = 1 + num_digs (n `div` 10)

reverse' :: Integer -> Integer
reverse' n = helper n 0 (num_digs n - 1)
  where
    helper :: Integer -> Integer -> Integer -> Integer
    helper n new pow = if n < 10
        then new + n * 10 ^ pow
        else helper (n `div` 10) (new + (n `mod` 10) * 10 ^ pow) (pow - 1)

prime :: Integer -> Bool
prime 1 = False
prime n = helper 2
  where
   helper :: Integer -> Bool
   helper k
      | fromIntegral k > sqrt (fromIntegral n) = True
      | n `mod` k == 0 = False
      | otherwise = helper (k + 1)

primeInterval :: Integer -> Integer -> Integer
primeInterval a b = helper a 0
  where
    helper :: Integer -> Integer -> Integer
    helper a sum
        | a > b = 0
        | prime a = a + helper (a + 1) (sum + a)
        | not (prime a) = helper (a + 1) sum

sumElems :: [Int] -> Int
sumElems xs  =
  if null xs
    then 0
    else head xs + sumElems (tail xs)

sumElems2 :: [Int] -> Int
sumElems2 = foldr sum  0 xs

removeElem :: Int -> [Int] -> [Int]
removeElem _ [] = []
removeElem e (x:xs) =
  if e == x
    then xs
    else x:removeElem e xs
