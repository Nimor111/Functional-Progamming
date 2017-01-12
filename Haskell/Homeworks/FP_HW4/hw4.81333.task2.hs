matrixCols :: Num a => [[a]] -> [a]
matrixCols [] = []
matrixCols [[]] = []
matrixCols ([]:_) = []
matrixCols xxs = map head xxs ++ matrixCols (map tail xxs)

matrixInfList :: Num a => [[a]] -> [a]
matrixInfList [] = []
matrixInfList [[]] = []
matrixInfList ([]:_) = []
matrixInfList xxs = cycle (matrixCols xxs)

main = do
  print (take 20 (matrixInfList [[1,2], [3,4], [5,6]]))
  print (take 20 (matrixInfList [[1,2,3,2,2,1,2,3], [1,2,3,4,5,6,7,8]])) 
  print (matrixInfList [])
  print (matrixInfList [[], []])
