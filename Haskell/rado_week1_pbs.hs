-- Truncatable primes

isPrime :: Integer -> Bool
isPrime x = prime 2
  where
    prime :: Integer -> Bool
    prime k
      | k == x = True
      | x `mod` k == 0 = False
      | otherwise = prime (k + 1)

truncatablePrime :: Integer -> Bool
truncatablePrime x
  | x == 0 = True
  | not (isPrime x) = False
  | otherwise = truncatablePrime (x `div` 10)


truncatablePrime2 0 = True
truncatablePrime2 x = isPrime x && truncatablePrime (x `div` 10)

-- Contains Digits
digitIn :: Int -> Int -> Bool
digitIn _ 0 = False
digitIn xs x = x == lastDigit || digitIn x butLast
  where
    lastDigit = xs `mod` 10
    butLast = xs `div` 10

 
containsDigits :: Int -> Int -> Bool
containsDigits _ 0 = True
containsDigits x y = digitIn lastDigit x && containsDigits x butLast
  where
    lastDigit = y `mod` 10
    butLast = y `div` 10
 

-- containsDigits2 d1 d2 = all (digitIn d1) ys
--   where
--     xs = toDigits d1
--     ys = toDigits d2

productOfDigits :: Int -> Int
productOfDigits 0 = 0
productOfDigits x = x `mod` 10 * productOfDigits (x `div` 10)

productOfDigits2 :: Int -> Int
productOfDigits2 = helper 1 -- apparently you can delete the argument
-- if it's in both funcs ( this was pod3 x = helper 1 x )
  where
    helper sum num
      | num == 1 = sum
      | otherwise = helper (sum * (num `mod` 10)) (num `div` 10)

-- helper function for interesting number
findDivisors :: Integer -> [Integer]
findDivisors n = [x | x <- [1..n-1], n `mod` x == 0]

interestingNumber :: Integer -> Bool
interestingNumber n = (n > 1) && sum (findDivisors (sum (findDivisors n))) == n 

quadrant :: Double -> Double -> Int
quadrant 0 0 = 0
quadrant x y
  | x > 0 && y > 0 = 1
  | x < 0 && y > 0 = 2
  | x < 0 && y < 0 = 3
  | otherwise = 4 
