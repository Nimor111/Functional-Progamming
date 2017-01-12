matrixCols :: Num a => [[a]] -> [[a]]
matrixCols [] = []
matrixCols [[]] = [[]]
matrixCols ([]:_) = [[]]
matrixCols xxs = map head xxs : matrixCols (map tail xxs)

keepsInside :: [[Int]] -> (Int -> Int) -> [[Int]]
keepsInside xxs f = filter (not . null) kept
  where kept = map (\ x -> filter (\ y -> f y `elem` x) x) (matrixCols xxs)


main :: IO ()
main = do 
  print(keepsInside [[1,0,5], [-1,0,2]] (^2))
  print(keepsInside [] (^2))
  print(keepsInside [[]] (^2))
  print(keepsInside [[], []] (^2))
