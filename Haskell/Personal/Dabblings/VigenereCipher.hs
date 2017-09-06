module VigenereCipher (
  vEncode,
  -- vDecode
) where

import           Data.Char (chr, ord)

generateKeywordString :: String -> String -> String
generateKeywordString source target = helper source target target
  where
    helper :: String -> String -> String -> String
    helper [] _ _ = []
    helper (s:ss) target@(t:ts) ot
      | s == ' ' = ' ' : helper ss target ot
      | null ts = t : helper ss ot ot
      | otherwise = t : helper ss ts ot

getAlphabetKey :: Char -> Int
getAlphabetKey c = ord c - 65

getCorrespondingLetter :: Char -> Char -> Char
getCorrespondingLetter ' ' ' ' = ' '
getCorrespondingLetter sc tc
  | value > 90 = chr $ value - 26
  | otherwise = chr value
  where
    value = ord sc + getAlphabetKey tc

vEncode :: String -> String -> String
vEncode source key = foldr (\(x,y) acc -> getCorrespondingLetter x y : acc) [] letters
  where
    letters = zip source (generateKeywordString source key)
