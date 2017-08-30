module GlobRegex
  (
    globToRegex,
    matchesGlob
  ) where

import Text.Regex.Posix ((=~))
import Data.Char (toLower)

matchesGlob :: FilePath -> String -> Bool -> Bool
matchesGlob name pat sensitive 
  | sensitive = name =~ globToRegex' pat
  | otherwise = lowercase name =~ (lowercase . globToRegex') pat
  where 
    lowercase :: String -> String
    lowercase = map toLower
                                          
-- he anchor
globToRegex :: String -> String
globToRegex cs = "^" ++ globToRegex' cs ++ "$"

-- he match
globToRegex' :: String -> String
globToRegex' "" = ""

globToRegex' ('*':cs) = ".*" ++ globToRegex' cs 
globToRegex' ('?':cs) = "." ++ globToRegex' cs 
globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs 

globToRegex' ('[':c:cs) = '[' : c : charClass cs 
globToRegex' ('[':_) = error "unterminated character class"

globToRegex' (c:cs) = escape c ++ globToRegex' cs

-- but most importantly
-- he escap
escape :: Char -> String
escape c
  | c `elem` regexChars = '\\' : [c]
  | otherwise = [c]
  where
    regexChars = "\\+()^$.{}]|"

charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs 
charClass (c:cs) = c : charClass cs 
charClass [] = error "unterminated character class"
