module Lib where

import           HetList
import           HuffmanTree

mapElements :: [String] -> HetList
mapElements = foldr (\x acc -> acc ++ [Element x]) []

getInputFromFile :: FilePath -> IO HetList
getInputFromFile file = do
  content <- readFile file
  let elements = words content in
      return $ reverse $ mapElements elements

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
