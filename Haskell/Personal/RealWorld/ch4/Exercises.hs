module Exercises where

import Data.Maybe
import Data.Char
import SplitLines

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs 

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (x:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit [x] = Just []
safeInit (x:xs) = Just (x : fromJust (safeInit xs))

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith func (x:xs)
  | func x = takeWhile func (x:xs) : splitWith func (dropWhile func (x:xs))
  | otherwise = [x] : splitWith func xs

firstWordOfLines :: String -> [String]
firstWordOfLines [] = []
firstWordOfLines input = map (fromJust . safeHead . words) (splitLines input)

transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' ([]:_) = []
transpose' xxs = map head xxs : transpose' (map tail xxs)

transposeText :: String -> String
transposeText = unlines . transpose' . splitLines

-- originally (\x acc -> acc ++ x), hlint is awesome
concat' :: [[a]] -> [a]
concat' = foldr (flip (++)) []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' pred (x:xs)
  | pred x = x : takeWhile' pred xs 
  | otherwise = []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' pred = foldr (\x acc -> if pred x then x : acc else []) []

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' pred (x:xs) = foldr step [] (x:xs) ++ groupBy' pred (dropWhile (pred x) xs)
  where
    step x acc = [x : takeWhile (pred x) xs]

groupBy'' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy'' _ [] = []
groupBy'' pred (x:xs) = (x : takeWhile (pred x) xs) : (groupBy'' pred . dropWhile (pred x)) xs

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' pred xs = foldr (\x acc -> pred x || acc) False xs

cycle' :: [a] -> [a]
cycle' [] = []
cycle' xs = foldr (:) [] xs ++ cycle' xs 

words' :: String -> [String]
words' [] = []
words' (c:ss) = foldr step [] (c:ss) ++ words' (drop 1 (dropWhile (not . isSpace) ss))
  where
    step c _ = [c : takeWhile (not . isSpace) ss]

unlines' :: [String] -> String
unlines' [] = []
unlines' xs = foldr (\x acc -> x ++ "\n" ++ acc) "" xs
