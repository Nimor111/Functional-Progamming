type Product = (String, Double)

getName :: Product -> String
getName (x,_)  = x

getPrice :: Product -> Double
getPrice (_, x) = x

discount :: Double -> [Product] -> [Product]
discount z [] = []
discount z ((x,y):xs) = (x, y - z / 100 * y) : discount z xs

discount2 :: Double -> [Product] -> [Product]
discount2 z [] = []
discount2 z (x:xs) = (getName x, getPrice x - z / 100 * getPrice x) : discount z xs

data Vector2D = Vector2D Double Double

instance Show Vector2D where
  show (Vector2D x y) = concat [show x, ",", show y]

instance Eq Vector2D where
  (==) (Vector2D x y) (Vector2D x2 y2) = x == x2 && y == y2

addVectors :: Vector2D -> Vector2D -> Vector2D
addVectors (Vector2D x y) (Vector2D z t) = Vector2D (x + z) (y + t)

vectorLength :: Vector2D -> Double
vectorLength (Vector2D x y) = sqrt (x ^ 2 + y ^ 2)

data Book = Paper String Int | Online String Int Int deriving (Eq, Show)
type Library = [Book]

getPages :: Book -> Int
getPages (Paper _ pages ) = pages
getPages (Online _ _ pages) = pages

getTotalPages :: Library -> Int
getTotalPages = sum . map getPages

getOnlineCount :: Library -> Int
getOnlineCount [] = 0
getOnlineCount (Online{}:xs) = 1 + getOnlineCount xs
getOnlineCount (Paper{}:xs) = getOnlineCount xs

getPaperCount :: Library -> Int
getPaperCount [] = 0
getPaperCount (Paper{}:xs) = 1 + getPaperCount xs
getPaperCount (Online{}:xs) = getPaperCount xs


