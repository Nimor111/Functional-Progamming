data Tree a = Empty | Node a (Tree a) (Tree a)

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Node _ lt rt) = 1 + countNodes lt + countNodes rt

minCount :: Eq a => Tree a-> a -> Int
minCount Empty _ = 0
minCount (Node y lt rt) x
  | x == y = if countNodes lt < countNodes rt then countNodes lt + 1 else countNodes rt + 1
  | otherwise = minCount lt x + minCount rt x 

exampleTree :: Tree Int
exampleTree = Node 1
                (Node 2 
                (Node 3 Empty Empty)
                (Node 4 Empty Empty))
                Empty

main = print $ minCount exampleTree 1
