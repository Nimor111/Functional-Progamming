{-- 09/01/2017
 functional programming where I actually learn sth
--}

-- container type
data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

exampleTree :: Tree Int
exampleTree = Node 5
                (Node 2 
                (Node 1 Empty Empty)
                (Node 3 Empty Empty))
                (Node 6 Empty Empty)

makeLeaf :: a -> Tree a
makeLeaf x = Node x Empty Empty

countNodes :: Tree a -> Int
countNodes Empty  = 0 
countNodes (Node _ lt rt) = 1 + countNodes lt + countNodes rt

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ lt rt) = 1 + max (treeHeight lt) (treeHeight rt)

flattenTree :: Tree a -> [a]
flattenTree Empty = []
flattenTree (Node x lt rt) = flattenTree lt ++ [x] ++ flattenTree rt

treeFind :: Eq a => a -> Tree a -> Bool
treeFind _ Empty = False
treeFind x (Node y lt rt) = x == y || treeFind x lt || treeFind x rt

bstInsert :: Ord a => a -> Tree a -> Tree a
bstInsert x Empty = makeLeaf x
bstInsert x (Node y lt rt) = if x < y then Node y (bstInsert x lt) rt else Node y lt (bstInsert x rt) 

bstFind :: Ord a => a -> Tree a -> Bool
bstFind _ Empty = False
bstFind x (Node y lt rt)
  | x == y = True
  | x < y = bstFind x lt
  | otherwise = bstFind x rt

treeLevel :: Int -> Tree a -> [a]
treeLevel _ Empty = []
treeLevel 1 (Node x _ _) = [x]
treeLevel k (Node x lt rt) = treeLevel (k-1) lt ++ treeLevel (k-1) rt

treeLevels :: Tree a -> [[a]]
treeLevels Empty = []
treeLevels tree = helper 1 tree
  where helper :: Int -> Tree a -> [[a]]
        helper _ Empty = []
        helper k tree = if k == treeHeight tree + 1 then [] else treeLevel k tree : helper (k+1) tree 

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _ = False

isLeaf :: Tree a -> Bool
isLeaf Empty = False
isLeaf (Node _ Empty Empty) = True
isLeaf _ = False

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Node _ Empty Empty) = 1
countLeaves (Node _ lt rt) = countLeaves lt + countLeaves rt

successorsCount :: Tree a -> Int
successorsCount Empty = 0
successorsCount (Node _ Empty Empty) = 0
successorsCount (Node _ lt rt)
  | not (isEmpty lt) && not (isEmpty rt) = 2
  | not (isEmpty lt) || not (isEmpty rt) = 1

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Empty = Empty
treeMap f (Node x lt rt) = Node (f x) (treeMap f lt) (treeMap f rt)

levelSum :: Int -> Tree Int -> Int
levelSum _ Empty = 0
levelSum y tree = (sum . treeLevel y) tree

main = print $ levelSum 2 exampleTree
