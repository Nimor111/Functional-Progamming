module Sudoku 
  (solveSudoku,
   printBoard)
  where


import Data.List
import Data.Char
import Data.Maybe
import SudokuChecker

type Board = [[Int]]
type Row = [Int]
type Col = [Int]
type Group = [Int]
type Element = (Int, Int)
type Choice = Int
type Cell = Int
type Number = Int

main = do
  printBoard $ solveSudoku testBoard
  putStrLn "Let's check validity of solution: "
  print $ checkSudoku $ solveSudoku testBoard
  
printBoard :: Maybe Board -> IO () 
printBoard Nothing = putStrLn "Well, it doesn't have a solution."
printBoard (Just []) = return ()
printBoard (Just (r:b)) = do putStrLn (intersperse ' ' (map intToDigit r)) 
                             printBoard (Just b)

testBoard :: Board
testBoard = [[8,4,0,6,7,0,9,3,0],
             [0,5,0,0,8,0,0,6,0],
             [0,0,6,0,0,9,0,0,4],
             [2,9,0,5,1,0,3,7,0],
             [0,6,0,0,4,0,0,9,0],
             [0,0,3,0,0,2,0,0,5],
             [6,8,0,4,2,0,5,1,0],
             [0,1,0,0,9,0,0,2,0],
             [0,0,2,0,0,1,0,0,8]]

solveSudoku :: Board -> Maybe Board
solveSudoku b = (filterEmpty . boards (getZeros b)) b

getRowElements :: Board -> Number -> Row
getRowElements [] _ = []
getRowElements (r:b) n 
  | n == 0 = r
  | otherwise = getRowElements b (n - 1) 

getColElements :: Board -> Number -> Col
getColElements [] _ = []
getColElements ([]:_) _ = []
getColElements b n
  | n == 0 = map head b
  | otherwise = getColElements (map tail b) (n - 1)

getElementAtPosition :: Board -> Element -> Maybe Cell 
getElementAtPosition [] _ = Nothing
getElementAtPosition (r:b) (row, col)
  | row < 0 || col < 0 || row >= 9 || col >= 9 = Nothing
  | row == 0 = Just (r !! col)
  | otherwise = getElementAtPosition b (row - 1, col)

getThreesByLocation :: Board -> Element -> Group
getThreesByLocation b (row, col)
  | col >= 0 && col <= 2 && row >= 0 && row <= 2 = head (concat (buildAllThrees (transpose b)))
  | col >= 3 && col <= 5 && row >= 0 && row <= 2 = concat (buildAllThrees (transpose b)) !! 1
  | col >= 6 && col <= 8 && row >= 0 && row <= 2 = concat (buildAllThrees (transpose b)) !! 2
  | col >= 0 && col <= 2 && row >= 3 && row <= 5 = concat (buildAllThrees (transpose b)) !! 3
  | col >= 3 && col <= 5 && row >= 3 && row <= 5 = concat (buildAllThrees (transpose b)) !! 4
  | col >= 6 && col <= 8 && row >= 3 && row <= 5 = concat (buildAllThrees (transpose b)) !! 5
  | col >= 0 && col <= 2 && row >= 6 && row <= 8 = concat (buildAllThrees (transpose b)) !! 6
  | col >= 3 && col <= 5 && row >= 6 && row <= 8 = concat (buildAllThrees (transpose b)) !! 7
  | col >= 6 && col <= 8 && row >= 6 && row <= 8 = concat (buildAllThrees (transpose b)) !! 8
  
isValidChoice :: Board -> Element -> Choice -> Bool
isValidChoice b (row,col) c = notInRow && notInCol && notInGroup
  where
    notInRow = c `notElem` (b `getRowElements` row)
    notInCol = c `notElem` (b `getColElements` col)
    notInGroup = c `notElem` (b `getThreesByLocation` (row, col))

fillElement :: Board -> Choice -> Element -> Board
fillElement [] _ _ = []
fillElement (r:b) c (row, col)
  | row == 0 = (take col r ++ [c] ++ drop (col + 1) r) : fillElement b c (row - 1, col)
  | otherwise = r : fillElement b c (row - 1, col)

getZeros :: Board -> [Element]
getZeros b = [(row, col) | row <- [0..8], col <- [0..8], getElementAtPosition b (row, col) == Just 0]

boards :: [Element] -> Board -> [Board]
boards [] b = [b]
boards (e:es) b = concatMap (boards es . (\c -> fillElement b c e)) (filter (isValidChoice b e) [1..9])

filterEmpty :: [Board] -> Maybe Board
filterEmpty []         = Nothing
filterEmpty (b:boards) = Just b -- return first solution
