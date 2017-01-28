myLast :: [a] -> a
myLast [] = error "Empty list!"
myLast [x] = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "Empty list!"
myButLast [x] = error "One element!"
myButLast [x, y] = x
myButLast (x:xs) = myButLast xs

-- index from 1
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Empty list!"
elementAt (x:xs) 1 = x
elementAt (x:xs) y = elementAt xs (y - 1)

myLength :: [a] -> Int
myLength = foldr (\ x -> (+) 1) 0

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = myReverse xs == xs

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

-- flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
