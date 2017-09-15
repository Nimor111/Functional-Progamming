module HuffmanTree where

import           Data.List (nub, (\\))
import           HetList

data HuffTree a =
  Empty
  | LeafNode a Int
  | Node (HuffTree a) Int (HuffTree a)
  deriving Show

instance Eq (HuffTree a) where
  LeafNode _ a == LeafNode _ b = a == b
  LeafNode{} == Node{} = False
  LeafNode{} == Empty = False
  (Node _ a _) == (Node _ b _) = a == b
  Empty == Empty = True
  Node{} == Empty = False

instance (Eq a) => Ord (HuffTree a) where
  LeafNode _ a `compare` LeafNode _ b = a `compare` b
  LeafNode _ a `compare` (Node _ b _) = a `compare` b
  Node _ a _ `compare` LeafNode _ b =  a `compare` b
  (Node _ a _) `compare` (Node _ b _) = a `compare` b
  LeafNode a _ `compare` Empty = GT
  Empty `compare` LeafNode a _ = LT
  Empty `compare` Empty = EQ
  Empty `compare` Node{} = LT
  Node{} `compare` Empty = GT

transformInput :: HetList -> [(Element, Int)]
transformInput hs = nub $ foldr (\x acc -> (x, countOccurrences hs x) : acc) [] hs
  where
    countOccurrences :: HetList -> Element -> Int
    countOccurrences hs e = sum (map (\x -> if e == x then 1 else 0) hs)

buildFirstTrees :: [(a, Int)] -> [HuffTree a]
buildFirstTrees = foldr (\x acc -> uncurry LeafNode x : acc) []

combineTrees :: [HuffTree a] -> HuffTree a
combineTrees [] = Empty
combineTrees [x] = x
combineTrees [Node l a r, LeafNode v b] = Node (Node l a r) (a + b) (LeafNode v b)
combineTrees [LeafNode v b, Node l a r] = Node (LeafNode v b) (a + b) (Node l a r)
combineTrees [LeafNode l a, LeafNode r b] = Node (LeafNode l a) (a + b) (LeafNode r b)
combineTrees [Node l a r, Node l1 a1 r1] = Node (Node l a r) (a + a1) (Node l1 a1 r1)

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

buildCodes :: Maybe (HuffTree a) -> [(a, String)]
buildCodes = undefined

huffman :: HetList -> String
huffman = undefined
