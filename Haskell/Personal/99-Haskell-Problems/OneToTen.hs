module OneToTen 
  (pack,
   encode)
  where
-- for &&&

import Control.Arrow

-- Problem 1
myLast :: [a] -> a
myLast [] = error "Empty list!"
myLast [x] = x
myLast (x:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast [] = error "Empty list!"
myButLast [x] = error "One element!"
myButLast [x, y] = x
myButLast (x:xs) = myButLast xs

-- index from 1
-- Problem 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Empty list!"
elementAt (x:xs) 1 = x
elementAt (x:xs) y = elementAt xs (y - 1)

-- Problem 4
myLength :: [a] -> Int
myLength = foldr (\ x -> (+) 1) 0

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = myReverse xs == xs

-- Problem 7
data NestedList a = Elem a | List [NestedList a] deriving Show

getElemValue :: NestedList a -> a
getElemValue    (Elem i) = i
getElemValue    _ = error "Not Elem type"

getListValues :: NestedList a -> [a]
getListValues (List []) = []
getListValues (List (x:xs)) = getElemValue x : getListValues (List xs)

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem i) = [i]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : compress (dropWhile (== x) xs)

compress' :: Eq a => [a] -> [a]
compress' xs = foldr (\x acc -> if x == head acc then acc else x : acc) [last xs] xs

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (== x) xs) : (pack . dropWhile (== x)) xs

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (length &&& head) (pack xs)

-- function that, given an input n, returns "n+1" and "n*2", uses Arrows
-- calc1 :: Int -> (Int,Int)
-- calc1 = (+1) &&& (*2)
