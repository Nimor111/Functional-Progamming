module HuffmanTree where

import           Data.List (nub, (\\))
import           HetList

data HuffTree a =
  Empty
  | Node (HuffTree a) Int (HuffTree a)
  deriving Show

data HuffMinTuple a =
  TEmpty
  | Single (HuffTree a)
  | Tuple (HuffTree a, HuffTree a)

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

combineTrees :: [HuffTree a] -> HuffTree a
combineTrees [] = Empty
combineTrees [x] = x
combineTrees [Node l a r, Node l1 a1 r1] = Node (Node l1 a1 r1) (a + a1) (Node l a r)

findMinTrees :: Eq a => [HuffTree a] -> [HuffTree a]
findMinTrees [] = []
findMinTrees [x] = [x]
findMinTrees hs = [minimum hs, minimum $ removeMin hs]
  where
    removeMin :: Eq a => [HuffTree a] -> [HuffTree a]
    removeMin hs = hs \\ [minimum hs]

buildTree :: Eq a => [HuffTree a] -> Maybe (HuffTree a)
buildTree []  = Nothing
buildTree [x] = Just x
buildTree hs  = buildTree (combineTrees mins : removeMins hs mins)
  where
    removeMins :: Eq a => [HuffTree a] -> [HuffTree a] -> [HuffTree a]
    removeMins [] _  = []
    removeMins hs ss = hs \\ ss
    mins = findMinTrees hs

huffman :: HetList -> String
huffman = undefined
