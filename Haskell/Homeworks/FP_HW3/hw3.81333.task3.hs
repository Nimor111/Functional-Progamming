import Data.List

extractRLE :: (Eq a) => [(a,Int)] -> [a]
extractRLE = concatMap (\ x -> [fst x | y <- [1..(snd x)]])

compressRLE :: (Eq a) => [a] -> [(a,Int)]
compressRLE xs = [(head x, length x) | x <- group xs]

getRLE :: (Eq a) => [(a,Int)] -> Int -> a
getRLE xs x = fst (concatMap (\ l -> [(fst l, 1) | x <- [1..(snd l)]]) xs !! x)

main :: IO ()
main = do 
  print $ getRLE [('m',1),('i',1),('s',2),('i',1),('s',2),('i',1),('p',2),('i',1)] 12
  print $ extractRLE [('m',1),('i',1),('s',2),('i',1),('s',2),('i',1),('p',2),('i',1)] 
  print $ compressRLE "mississipi"
