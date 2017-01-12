module SudokuChecker
  (checkSudoku,
   buildAllThrees)
  where

import Data.List
import Data.Maybe

type Board = [[Int]]

checkRows :: Board -> Bool
checkRows [] = True
checkRows (xs:xxs)
  | sort xs /= [1,2,3,4,5,6,7,8,9] = False
  | otherwise = checkRows xxs

checkCols :: Board -> Bool
checkCols ([]:_) = True
checkCols xxs
  | sort (map head xxs) /= [1,2,3,4,5,6,7,8,9] = False
  | otherwise = checkCols (map tail xxs)

buildThrees :: Board -> [[Int]]
buildThrees [] = []
buildThrees xxs = buildGroup (take 3 three) : buildThrees (drop 3 three)
  where three = map (take 3) xxs

buildAllThrees :: Board -> [[[Int]]]
buildAllThrees ([]:_) = []
buildAllThrees xxs = buildThrees (map (take 3) xxs) : buildAllThrees (map (drop 3) xxs)

checkThrees :: Board -> Bool
checkThrees xxs = all (==True) (map checkGroup (concat (buildAllThrees xxs)))

buildGroup :: Board -> [Int]
buildGroup ([]:_) = []
buildGroup xxs = map head xxs ++ buildGroup (map tail xxs)

checkGroup :: [Int] -> Bool
checkGroup xs = sort xs == [1,2,3,4,5,6,7,8,9]

checkSudoku :: Maybe Board -> Bool
checkSudoku xxs = checkRows (fromJust xxs) && checkCols (fromJust xxs) && checkThrees (fromJust xxs)

main = do 
  print $ checkSudoku (Just [[1,1,1,1,1,1,1,1,1],
                             [2,2,2,2,2,2,2,2,2],
                             [3,3,3,3,3,3,3,3,3],
                             [4,4,4,4,4,4,4,4,4],
                             [5,5,5,5,5,5,5,5,5],
                             [6,6,6,6,6,6,6,6,6],
                             [7,7,7,7,7,7,7,7,7],
                             [8,8,8,8,8,8,8,8,8],
                             [9,9,9,9,9,9,9,9,9]])
  print $ checkSudoku (Just [[8,4,1,6,7,5,9,3,2],
                             [3,5,9,2,8,4,1,6,7],
                             [7,2,6,1,3,9,8,5,4],
                             [2,9,4,5,1,8,3,7,6],
                             [5,6,8,3,4,7,2,9,1],
                             [1,7,3,9,6,2,4,8,5],
                             [6,8,7,4,2,3,5,1,9],
                             [4,1,5,8,9,6,7,2,3],
                             [9,3,2,7,5,1,6,4,8]])
