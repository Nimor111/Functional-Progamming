data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

-- Takes a type constructor not a concrete type
-- applies function to constructors of that type
instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

-- Either type, Either a b (takes two args, we have to partially apply)
-- We apply fmap to one type, so one of the Either types will be transformed
-- the other stays the same, Left here is like an empty box with an error
-- message written on the side

data Either' a b = Left' a | Right' b

instance Functor (Either' a) where
  fmap f (Right' x) = Right' (f x)
  fmap f (Left' x) = Left' x

main :: IO ()
main =
    print $ fmap (*2) exampleTree
      where
        exampleTree :: Tree Integer
        exampleTree = Node 1
                      (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty))
                      (Node 5 (Node 6 Empty Empty) (Node 7 Empty Empty))
