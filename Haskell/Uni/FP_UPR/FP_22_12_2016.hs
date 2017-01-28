import Data.List
import Data.Char
import Data.List.Split

main :: IO()
main = do
 -- print (grouping (mapNums("a123b2c56")))
 print(sumNumbers "a123b2c56")
 print(matrixSum [[1,2,3], [4,5,6]] [[4,5,6], [1,2,3]])
 print(transposeMatrix [[1,2], [4,5]])
 --print(zipMult [1,2,3] [4,5,6])
 
sumNumbers :: String -> Int
sumNumbers xs = sum (map fromDigits (splitWhen (> 10) (mapNums "a123b2c56")))

fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
  where addDigit num d = 10 * num + d

mapNums :: String -> [Int]
mapNums = map (\ x -> ord x - 48 )

matrixSum :: [[Int]] -> [[Int]] -> [[Int]]
matrixSum xxs [] = xxs
matrixSum [] xxs = xxs
matrixSum (x:xxs) (y:yys) = zipWith (+) x y : matrixSum xxs yys

transposeMatrix :: [[Int]] -> [[Int]]
transposeMatrix ([]:_) = []
transposeMatrix xxs = map head xxs : transposeMatrix (map tail xxs)

--multMatrix 

--zipMult :: [[a]] -> [[a]]
--zipMult (x:xxs) = zip x (zipMult xss)







