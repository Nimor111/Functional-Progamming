module Lib (
  someFunc
) where

import           HetList
import           HuffmanTree

mapElements :: [String] -> HetList
mapElements = foldr (\x acc -> acc ++ [Element x]) []

getInputFromFile :: FilePath -> IO ()
getInputFromFile file = do
  content <- readFile file
  let elements = words content in
      print $ mapElements elements

getInput :: IO HetList
getInput = do
  x <- getLine
  case x of
    "exit" -> return []
    _      -> fmap (Element x: ) getInput

someFunc :: IO ()
someFunc = do
  xs <- getInput
  print $ huffman xs
