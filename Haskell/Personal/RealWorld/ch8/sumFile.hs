{- 
  Reads a file of numbers and outputs their sum
  read does not need a type constraint here, it knows they are numbers
  because of sum
-}

main = do 
  contents <- getContents
  print $ sumFile contents
    where
      sumFile = sum . map read . words
