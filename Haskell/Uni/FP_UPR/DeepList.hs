import Data.List

data DeepList = NilList | List [Int] | LevelList [DeepList] deriving Show

getNums :: DeepList -> [Int]
getNums NilList = []
getNums (List xs) = xs
getNums (LevelList xs) = concatMap getNums xs

intersect' :: DeepList -> DeepList -> [Int]
intersect' d1 d2 = getNums d1 `intersect` getNums d2

union' :: DeepList -> DeepList -> [Int]
union' d1 d2 = getNums d1 `union` getNums d2

main = do
  print $ getNums $ LevelList [List [1,2,3], LevelList [List [1,2,3]]]
  print $ getNums $ List [1,2,3]
