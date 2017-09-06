module VigenereCipher (
  vEncode,
  -- vDecode
) where

import           Data.Char (chr, isSpace, ord)

generateKeywordString :: String -> String -> String
generateKeywordString source target = helper source target target
  where
    helper :: String -> String -> String -> String
    helper [] _ _ = []
    helper (s:ss) target@(t:ts) orig
      | isSpace s = ' ' : helper ss target orig
      | null ts = t : helper ss orig orig
      | otherwise = t : helper ss ts orig

getAlphabetKey :: Char -> Int
getAlphabetKey c = ord c - 65

getCorrespondingLetter :: Char -> Char -> Char
getCorrespondingLetter ' ' ' ' = ' '
getCorrespondingLetter sc tc
  | value > 90 = chr $ value - 26
  | otherwise = chr value
  where
    value = ord sc + getAlphabetKey tc

-- using folds
vEncode :: String -> String -> String
vEncode source key = foldr (\(x,y) acc -> getCorrespondingLetter x y : acc) [] letters
  where
    letters = zip source (generateKeywordString source key)

-- using list monad bind
vEncode' :: String -> String -> String
vEncode' source key = letters >>= (\(x, y) -> [getCorrespondingLetter x y])
  where
    letters = zip source (generateKeywordString source key)
