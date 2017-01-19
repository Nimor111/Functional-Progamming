import Data.List

main = do
  let store1 = [("bread", 1), ("milk", 2.5), ("lamb", 10),
                ("cheese", 5), ("butter", 2.3)]
  let store2 = [("bread", 1), ("cheese", 2.5), ("bread", 1),
                ("cheese", 5.5), ("butter", 2.3)]
  print $ minDistance [(1,2,3), (3,4,5), (4,5,6)]
  print $ cheaperAlternative store2

  print $ maxCount exampleTree 4
  -- let fn = maximize [\x -> x * x * x, (+1)]
  -- print $ fn 0.5
  -- print $ fn (-2)
  where
      exampleTree = Node 4
                    (Node 5 (Node 6 Empty Empty) (Node 7 Empty Empty))
                    (Node 8 (Node 9 Empty Empty) Empty)


-- problem 1 sumUnique
occurrences :: Int -> [Int] -> Int
occurrences _ [] = 0
occurrences y (x:xs)
  | y == x = 1 + occurrences y xs
  | otherwise = occurrences y xs

sumUnique :: [[Int]] -> Int
sumUnique [] = 0
sumUnique ([]:_) = 0
sumUnique xs = sum $ map (sum . (\x -> (filter (\y -> occurrences y x == 1) x))) xs
    
-- problem 2 - Products

type Product = (String, Double)
type StoreAvailability = [Product]

-- 1)
closestToAverage :: StoreAvailability -> String
closestToAverage [] = "I don't think so."
closestToAverage store = closest 
  where
    prices = [snd x | x <- store]
    beforeLast :: [Double] -> Double 
    beforeLast [] = error "Empty list!"
    beforeLast [x] = error "One element!"
    beforeLast [x, y] = x
    beforeLast (x:xs) = beforeLast xs
    maxClosest = (beforeLast . sort) prices 
    closest = head [fst x | x <- store, snd x == maxClosest]

occurrenceOfProduct :: Product -> StoreAvailability -> Int
occurrenceOfProduct _ [] = 0
occurrenceOfProduct product (p:store)
  | fst product == fst p = 1 + occurrenceOfProduct product store
  | otherwise = occurrenceOfProduct product store

removeBiggerOccurrences :: Product -> StoreAvailability -> StoreAvailability
removeBiggerOccurrences _ [] = []
removeBiggerOccurrences product (p:store)
  | fst product == fst p && snd product >= snd p = p : removeBiggerOccurrences product store
  | otherwise = removeBiggerOccurrences product store

cheaperAlternative :: StoreAvailability -> Int
cheaperAlternative [] = error "I don't think so."
cheaperAlternative store = numberOfCheaper 
  where 
    elements = [x | x <- store, occurrenceOfProduct x store > 1]
    numberOfCheaper = length $ nubFirst $ filterEls elements
    filterEls :: StoreAvailability -> StoreAvailability
    filterEls [] = [] 
    filterEls (p:store) = removeBiggerOccurrences p store ++ filterEls store
    nubFirst :: StoreAvailability -> StoreAvailability
    nubFirst [] = []
    nubFirst (p:store) = head [x | x <- store, fst p == fst x] : nubFirst store

-- problem 3 - minDistance
buildDistances :: (Double, Double, Double) -> [(Double, Double, Double)] -> [Double]
buildDistances _ [] = [] 
buildDistances (x, y, z) ((xx, yy, zz):xs) =
  ((x - xx) * (x - xx) + (y - yy) * (y - yy) + (z - zz) * (z - zz)) : buildDistances (x,y,z) xs

minDistance :: [(Double, Double, Double)] -> Maybe [Double]
minDistance [] = Nothing
minDistance xs = Just (allDistances xs)
  where
    allDistances :: [(Double, Double, Double)] -> [Double]
    allDistances [] = []
    allDistances (x:xs) = buildDistances x xs ++ allDistances xs

-- problem 4 - maximize funcs
maximize :: (Ord a, Num a) => [a -> a] -> (a -> a)
maximize = max
  where
    max :: (Ord a, Num a) => [a -> a] -> a -> a
    max [] _ = error "Empty list"
    max xs x = head [g x| g <- xs, abs (g x) == maximum (map (abs . (\f -> f x)) xs)]

-- problem 5 - max subtree
data BT = Empty | Node Int BT BT
maxCount :: BT -> Int -> Int
maxCount Empty _ = 0
maxCount (Node x lt rt) y 
  | x == y = checkCount
  | otherwise = maxCount lt y + maxCount rt y 
  where
    count :: BT -> Int
    count Empty = 0
    count (Node _ lt rt) = 1 + count lt + count rt
    checkCount = if count lt > count rt
                 then count lt + 1
                 else count rt + 1

