data NTree = NilT | 
  Node Int NTree NTree

emptyTree :: NTree -> Bool
emptyTree NilT = True
emptyTree _    = False

leftTree :: NTree -> NTree
leftTree NilT = NilT
leftTree (Node _ lt _) = lt

rightTree :: NTree -> NTree
rightTree NilT = NilT
rightTree (Node _ _ rt) = rt

printTree :: NTree -> String
printTree NilT = " "
printTree (Node x lt rt) = printTree lt ++ show x ++ printTree rt

nodes :: NTree -> Int
nodes NilT = 0
nodes (Node _ lt rt) = 1 + nodes lt + nodes rt

depth :: NTree -> Int
depth NilT = 0
depth (Node _ NilT NilT) = 0
depth (Node _ lt rt) = 1 + max (depth lt) (depth rt)

levelK :: NTree -> Int -> [Int]
levelK NilT _ = []
levelK (Node x lt rt) 0 = [x]
levelK (Node _ lt rt) level = levelK lt (level - 1) ++ levelK rt (level - 1)

valueT :: NTree -> Int
valueT NilT = -1
valueT (Node x _ _) = x

getEdges :: NTree -> [(Int, Int)]
getEdges NilT = []
getEdges (Node x NilT NilT) = [] 
getEdges (Node x lt rt) = (x, valueT lt) : (x, valueT rt) :
                          getEdges lt ++ getEdges rt

isEdge :: NTree -> (Int, Int) -> Bool
isEdge NilT _ = False
isEdge tree edge = edge `elem` getEdges tree

consElList :: Int -> [Int] -> [(Int,Int)]
consElList _ [] = []
consElList x (y:ys) = (x,y) : consElList x ys

consAllEls :: [Int] -> [Int] -> [(Int,Int)]
consAllEls _ [] = []
consAllEls [] ys = [] 
consAllEls (x:xs) ys = consElList x ys ++ consAllEls xs ys

edgesOn :: NTree -> Int -> [(Int, Int)]
edgesOn tree k = filter (isEdge tree)
                 (consAllEls (levelK tree (k-1)) (levelK tree k))

main = do
  -- print $ printTree tree
  -- print $ nodes tree
  -- print $ depth tree
  print $ levelK tree 1 -- expect 2
  -- print $ getEdges tree
  -- print $ isEdge tree (1,2)
  -- print $ isEdge tree (7,8)
  print $ edgesOn tree 2
  where 
    tree = Node 1
            (Node 2
                ( Node 4 NilT NilT ) 
                (Node 5 NilT (Node 7 NilT NilT)))
            (Node 3
                 (Node 6 (Node 8 NilT NilT) (Node 9 NilT NilT))
                 NilT)
