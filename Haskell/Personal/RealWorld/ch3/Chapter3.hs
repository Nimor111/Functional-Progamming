-- {-# LANGUAGE FlexibleContexts #-}

module Chapter3 where

import Data.List
import Control.Monad
import System.Random.Shuffle

data BinTree a = Node a (BinTree a) (BinTree a)
                | Empty deriving Show

data Direction = Acute | Obtuse | Straight deriving Show

instance Eq Direction where
  Acute == Acute       = True
  Obtuse == Obtuse     = True 
  Straight == Straight = True
  _ == _               = False

data Point2D = Point2D Double Double deriving Show

instance Eq Point2D where
  (Point2D x1 y1) == (Point2D x2 y2) = x1 == x2 && y1 == y2

-- Ex 1
len :: [a] -> Int
len = foldr (\ x -> (+) 1) 0

len' :: [a] -> Int
len' = sum . map (const 1)

-- Ex 2
mean :: (Fractional a) => [a] -> a
mean xs = sum xs / fromIntegral (len xs)

-- Ex 3
intoPalindrome :: [a] -> [a]
intoPalindrome xs = xs ++ reverse xs

-- Ex 4
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

--- Ex 5
compareLen :: [a] -> [a] -> Ordering
compareLen xs ys = compare (len xs) (len ys)

sortBySublists :: (Eq a, Ord a) => [[a]] ->[[a]]
sortBySublists = sortBy compareLen

-- Ex 6
intersperse' :: [String] -> Char -> String
intersperse' [] _ = []
intersperse' [x] _ = x
intersperse' (x:xs) sep = x ++ [sep] ++ intersperse' xs sep

-- Ex 7
height :: BinTree a -> Int
height Empty = 0
height (Node _ Empty Empty) = 1
height (Node _ l r) = height l + height r

-- Ex 8
angleBetweenPoints :: Point2D -> Point2D -> Point2D -> Direction
angleBetweenPoints (Point2D x1 y1) (Point2D x2 y2) (Point2D x3 y3)
  | angle > 0 = Acute
  | angle < 0 = Obtuse
  | angle == 0 = Straight
  | otherwise = error "Not a valid angle!"
  where
    angle = (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1)

-- Ex 9
directionOfTriples :: [Point2D] -> [Direction]
directionOfTriples [] = []
directionOfTriples [x] = []
directionOfTriples [x, y] = []
directionOfTriples (x:y:z:ps) = angleBetweenPoints x y z : directionOfTriples (y:z:ps)

-- Ex 10
quickSort :: (Eq a, Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort left ++ mid ++ quickSort right
  where
    left  = filter (< x) xs
    right = filter (> x) xs
    mid   = filter (== x) xs ++ [x]

swap :: (a, a) -> (a, a)
swap (x, y) = (y, x)

sqDirOfVector :: Point2D -> Point2D -> Double
sqDirOfVector (Point2D x1 y1) (Point2D x2 y2) = (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)

compareByOrientation :: Point2D -> Point2D -> Point2D -> Ordering
compareByOrientation p1 p2 p3
  | angle == Straight = if sqDirOfVector p1 p3 >= sqDirOfVector p1 p2 then LT else GT
  | angle == Acute = GT
  | angle == Obtuse = LT
  where
    angle = angleBetweenPoints p1 p2 p3

getYCoord :: Point2D -> Double
getYCoord (Point2D _ y) = y

getXCoord :: Point2D -> Double
getXCoord (Point2D x _) = x

findMinByCoord :: [Point2D] -> (Point2D -> Double) -> [Point2D]
findMinByCoord ps coord = filter (\x -> coord x == minimum (map coord ps)) ps

findBottomestPoint :: [Point2D] -> Point2D
findBottomestPoint ps
  | len bottomPoints > 1 = head $ findMinByCoord ps getXCoord
  | otherwise = head bottomPoints
  where
    bottomPoints = findMinByCoord ps getYCoord

processPointsInModifiedList :: [Point2D] -> [Point2D] -> [Point2D]
processPointsInModifiedList [] ss = ss
processPointsInModifiedList (p:ps) (top:next:ss)
  | angleBetweenPoints next top p /= Obtuse = processPointsInModifiedList (p:ps) (next:ss)
  | otherwise = processPointsInModifiedList ps (p:top:next:ss)

placeElementAtHead :: Eq a => [a] -> a -> [a]
placeElementAtHead [] _ = []
placeElementAtHead xs x = x : filter (/= x) xs

removePointsWithSameAngles :: [Point2D] -> Point2D -> [Point2D]
removePointsWithSameAngles []     _    = []
removePointsWithSameAngles [x]    _    = []
removePointsWithSameAngles [x, y] _    = []
removePointsWithSameAngles (x:y:xs) p0
  | angleBetweenPoints p0 x y == Straight = removePointsWithSameAngles (y:xs) p0
  | otherwise = x : removePointsWithSameAngles (y:xs) p0

grahamScan :: [Point2D] -> [Point2D]
grahamScan points = processPointsInModifiedList (drop 3 modified) (take 3 modified)
  where
    modified = bottom : sorted
    -- modified = bottom : removePointsWithSameAngles sorted bottom
    sorted = sortBy (flip (compareByOrientation bottom)) (drop 1 (placeElementAtHead points bottom))
    bottom = findBottomestPoint points

-- Dabblings / Use shuffleM from System.Random.Shuffle
mquickSort :: (Monad m, Eq a, Ord a) => [a] -> m [a]
mquickSort xs = return (quickSort xs)

sortShuffledList :: (Monad m, Ord a) => m [a] -> m [a]
sortShuffledList xs = xs >>= mquickSort
