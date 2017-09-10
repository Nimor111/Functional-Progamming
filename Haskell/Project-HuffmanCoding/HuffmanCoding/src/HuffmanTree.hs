module HuffmanTree where

import           Data.List (nub)
import           HetList

data HuffTree a =
  Empty
  | Node (HuffTree a) Int (HuffTree a)
  deriving Show

instance Eq (HuffTree a) where
  (Node _ a _) == (Node _ b _) = a == b
  Empty == Empty = True
  Node{} == Empty = False
  Empty == Node{} = False

instance (Eq a) => Ord (HuffTree a) where
  (Node _ a _) `compare` (Node _ b _) = a `compare` b
  Empty `compare` Empty = EQ
  Empty `compare` Node{} = LT
  Node{} `compare` Empty = GT

transformInput :: HetList -> [(Element, Int)]
transformInput hs = nub $ foldr (\x acc -> (x, countOccurrences hs x) : acc) [] hs
  where
    countOccurrences :: HetList -> Element -> Int
    countOccurrences hs e = sum (map (\x -> if e == x then 1 else 0) hs)

buildFirstTrees :: [(Element, Int)] -> [HuffTree a]
buildFirstTrees = foldr ((\x acc -> Node Empty x Empty : acc) .snd) []

combineTrees :: HuffTree a -> HuffTree a -> HuffTree a
combineTrees (Node l a r) (Node l1 a1 r1) = Node (Node l a r) (a + a1) (Node l1 a1 r1)

findMinTrees :: [HuffTree a] -> [HuffTree a]
findMinTrees hs = undefined

huffman :: HetList -> String
huffman = undefined
