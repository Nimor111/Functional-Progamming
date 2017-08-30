module GlobRegex
  (
    matchesGlob,
    -- namesMatching
  ) where

import Text.Regex.Posix ((=~))
import Data.Char (toLower)
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)
import System.FilePath ((</>), isPathSeparator, dropTrailingPathSeparator, splitFileName)
import Control.Monad (forM)
import Control.Exception (handle)

type GlobError = String

-- how many files and dirs match glob in a directory
-- namesMatching :: String -> IO [FilePath]
-- namesMatching pat
--   | not ( isPattern pat ) = do
--     exists <- doesNameExist pat
--     return [pat | exists]
--   | otherwise = do
--     case splitFileName pat of 
--       ("./", baseName) -> do
--         curDir <- getCurrentDirectory
--         listMatches curDir baseName
--       (dirName, baseName) -> do
--         dirs <- if isPattern dirName
--                then namesMatching (dropTrailingPathSeparator dirName)
--                else return [dirName]
--         let listDir = if isPattern baseName
--                       then listMatches
--                       else listPlain
--         pathNames <- forM dirs $ \dir -> do
--                         baseNames <- listDir dir baseName
--                         return (map (dir </>) baseNames)
--         return (concat pathNames)

-- doesNameExist :: FilePath -> IO Bool
-- doesNameExist name = do
--   fileExists <- doesFileExist name
--   return (fileExists || doesDirectoryExist name)

-- listMatches :: FilePath -> String -> IO [FilePath]
-- listMatches dirName pat = do
--   dirName' <- if null dirName
--              then getCurrentDirectory
--              else return dirName
--   handle (const (return [])) $ do
--     names <- getDirectoryContents dirName'
--     let names' = if isHidden pat
--                  then filter isHidden names
--                  else filter ( not . isHidden ) names
--     return (filter (match' pat) names')
--     where
--       match' :: String -> FilePath -> Bool
--       match' pat cs = case isPathSeparator '/' of
--                         True -> matchesGlob cs pat True
--                         False -> matchesGlob cs pat False

-- isHidden :: String -> Bool
-- isHidden ('.':_) = True
-- isHidden _       = False

-- listPlain :: FilePath -> String -> IO [FilePath]
-- listPlain dirName baseName = do
--   exists <- if null baseName
--            then doesDirectoryExist dirName
--            else doesNameExist (dirName </> baseName)
--   return (if exists then [baseName] else [])

-- -- check if string contains pattern characters, if not, simply check filename
-- isPattern :: String -> Bool
-- isPattern = any (`elem` "[*?")

matchesGlob :: FilePath -> String -> Bool -> Either GlobError Bool
matchesGlob name pat sensitive 
  | sensitive = case globToRegex' pat of 
                  Left err -> Left err
                  Right result -> Right (name =~ result)
  | otherwise = case globToRegex' pat of
                  Left err -> Left err
                  Right result -> Right (lowercase name =~ lowercase result)
  where 
    lowercase :: String -> String
    lowercase = map toLower
                                          
-- he anchor
globToRegex :: String -> Either GlobError String
globToRegex cs = case globToRegex' cs of
                   Left err -> Left err
                   Right result -> Right ("^" ++ result ++ "$")

-- he match
globToRegex' :: String -> Either GlobError String
globToRegex' "" = Right ""

globToRegex' ('*':cs) = case globToRegex' cs of
                          Left err -> Left err
                          Right result -> Right (".*" ++ result)
globToRegex' ('?':cs) = case globToRegex' cs of
                          Left err -> Left err
                          Right result -> Right ("." ++ result)
globToRegex' ('[':'!':c:cs) = case charClass cs of
                                Left err -> Left err
                                Right result -> Right ("[^" ++ c : result)

globToRegex' ('[':c:cs) = case charClass cs of
                            Left err -> Left err
                            Right result -> Right ('[' : c : result)
globToRegex' ('[':_) = Left "unterminated character class"

globToRegex' (c:cs) = case globToRegex' cs of
                        Left err -> Left err
                        Right result -> Right (escape c ++ result)

-- but most importantly
-- he escap
escape :: Char -> String
escape c
  | c `elem` regexChars = '\\' : [c]
  | otherwise = [c]
  where
    regexChars = "\\+()^$.{}]|"

charClass :: String -> Either GlobError String
charClass (']':cs) = case globToRegex' cs of
                       Left err -> Left err
                       Right result -> Right (']' : result)
charClass (c:cs) = case charClass cs of
                     Left err -> Left err
                     Right result -> Right (c : result)
charClass [] = Left "unterminated character class"
