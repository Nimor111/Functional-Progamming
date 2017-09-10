module HuffmanTree where

import           HetList

data HuffTree a =
  Empty
  | Node (HuffTree a) (a, Int) (HuffTree a)
  deriving Show

transformInput :: HetList -> [(a, Int)]
transformInput = undefined

buildFirstTree :: [(a, Int)] -> [HuffTree a]
buildFirstTree = undefined

combineTrees :: HuffTree a -> HuffTree a -> HuffTree a
combineTrees = undefined

huffman :: HetList -> String
huffman = undefined
