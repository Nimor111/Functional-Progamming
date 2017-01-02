zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip xs ys

fst' :: (a, b) -> a
fst' (x, _) = x

snd' :: (a, b) -> b
snd' (_, x) = x

-- Типови класове
-- създаване на типове
-- типов синоним type String = [Char]
-- type Matrix = [[Int]]
-- Rule - Value constructors used for pattern matching
-- Type is used in function signatures
-- Show:: Show a => a -> String

data Shape = Circle | Triangle | Rectangle -- deriving Show

instance Show Shape where
  show Circle = "Circle"
  show Triangle = "Triangle"
  show Rectangle = "Rectangle"

isCircle :: Shape -> Bool
isCircle Circle = True
isCircle _ = False

isTriangle :: Shape -> Bool
isTriangle Triangle = True
isTriangle _ = False

isRectangle :: Shape -> Bool
isRectangle Rectangle = True
isRectangle _ = False

instance Eq Shape where
  (==) Circle Circle = True
  (==) Triangle Triangle = True
  (==) Rectangle Rectangle = True
  (==) _         _         = False

data Vector2D = Vector2D Double Double

data Maybe a = Nothing | Just a

instance Show Vector2D where
  show (Vector2D x y) = "Vector2D :: " ++ concat ["(", show x, ",", show y, ")"]
