import OneToTen
import Control.Applicative
import Data.Maybe
import Data.List

data Operation a = Multiple Int a | Single a deriving Show


-- Problem 11
encodeModified :: Eq a => [a] -> [Operation a]
encodeModified xs = map (\x -> if length x == 1
                              then Single (head x)
                              else Multiple (length x) (head x))
                        (pack xs)

-- Problem 12
decodeModified :: Eq a => [Operation a] -> [a]
decodeModified [] = []
decodeModified (Multiple x y : xs) = replicate x y ++ decodeModified xs
decodeModified (Single x : xs) = x : decodeModified xs

decodeModified' :: Eq a => [Operation a] -> [a]
decodeModified' = foldr (\x acc -> helper x ++ acc) []
  where
    helper :: Eq a => Operation a -> [a]
    helper (Multiple x y) = replicate x y
    helper (Single x) = [x]

-- Problem 13
encodeDirect :: Eq a => [a] -> [Operation a]
encodeDirect [] = []
encodeDirect [x] = [Single x]
encodeDirect (x:xs)
  | x == head xs = Multiple (numberOfMultiples x xs + 1) x : encodeDirect (drop (numberOfMultiples x xs) xs)
  | x /= head xs = Single x : encodeDirect xs
  where
    numberOfMultiples :: Eq a => a -> [a] -> Int 
    numberOfMultiples _ [] = 0
    numberOfMultiples y (x:xs)
      | y == x = 1 + numberOfMultiples y xs
      | y /= x = 0

-- Problem 14
dupli :: [a] -> [a]
dupli = foldr (\x acc -> x : x : acc) []

dupli' :: [a] -> [a]
dupli' [] = []
dupli' (x:xs) = x : x : dupli' xs

-- list monad wohoo
dupli'' :: [a] -> [a]
dupli'' xs = xs >>= (\x -> [x, x])

-- applicative functor
dupli''' :: [a] -> [a]
dupli''' = (<**> [id, id])

-- Problem 15
repli :: Int -> [a] -> [a]
repli num = foldr (\x acc -> prepend num x ++ acc) []
  where
    prepend :: Int -> a -> [a]
    prepend 1 x = [x]
    prepend n x = x : prepend (n - 1) x

-- Problem 16
removeAt :: Eq a => [a] -> Int -> [a]
removeAt [] _ = []
removeAt (x:xs) 1 = xs
removeAt (x:xs) n = x : removeAt xs (n - 1)

-- solution from website, amazing
dropEvery :: Eq a => [a] -> Int -> [a]
dropEvery xs n = map fst $ filter (\(x,i) -> i `mod` n /= 0) $ zip xs [1..]

dropEvery' :: Eq a => [a] -> Int -> [a]
dropEvery' xs n = foldr (\(x, i) acc -> if i `mod` n /= 0 then x : acc else acc) [] $ zip xs [1..]

-- Problem 17
split :: Eq a => [a] -> Int -> ([a], [a])
split xs n = (first xs n, second xs n)
  where
    first :: [a] -> Int -> [a]
    first xs 0 = [] 
    first (x:xs) n = x : first xs (n-1)

    second :: [a] -> Int -> [a]
    second xs 0 = xs
    second (x:xs) n = second xs (n-1)
