module IntParse where

import Data.Char (digitToInt, isDigit)
import Data.List (elem)

type ErrorMessage = String

isNegative :: String -> Bool
isNegative str = '-' `elem` str

hasNonDigit :: String -> Bool
hasNonDigit [] = False
hasNonDigit (x:xs)
  | (==) x '-' = hasNonDigit xs
  | not (isDigit x) = True
  | otherwise = hasNonDigit xs

parseDigits :: String -> Either ErrorMessage Integer
parseDigits [] = Left "Empty string provided"
parseDigits xs
  | hasNonDigit xs = Left "Has a non digit character!"
  | isNegative xs = Right (negate num)
  | otherwise = Right num
  where
    num = foldl (\acc x -> acc * 10 + fromIntegral (digitToInt x)) 0 str
    str = if isNegative xs then drop 1 xs else xs
