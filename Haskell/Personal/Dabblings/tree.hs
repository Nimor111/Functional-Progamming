data BinaryTree a =
  Empty
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

instance Functor BinaryTree where
  fmap _ Empty               = Empty
  fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert value Empty = Node Empty value Empty
insert value (Node left a right)
  | value == a = Node left a right
  | value < a = Node (insert value left) a right
  | value > a = Node left a (insert value right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Empty               = Empty
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

preorder :: BinaryTree a -> [a]
preorder Empty                  = []
preorder (Node left root right) = [root] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Empty                  = []
inorder (Node left root right) = inorder left ++ [root] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Empty                  = []
postorder (Node left root right) = postorder left ++ postorder right ++ [root] ++ postorder left

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Empty = acc
foldTree f acc tree  = foldr f acc $ inorder tree

tree :: BinaryTree Int
tree = Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty)
