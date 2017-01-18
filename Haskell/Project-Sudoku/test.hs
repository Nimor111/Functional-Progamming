import Sudoku
import SudokuChecker

boardsOne = [[0,0,0,2,6,0,7,0,1],
             [6,8,0,0,7,0,0,9,0],
             [1,9,0,0,0,4,5,0,0],
             [8,2,0,1,0,0,0,4,0],
             [0,0,4,6,0,2,9,0,0],
             [0,5,0,0,0,3,0,2,8],
             [0,0,9,3,0,0,0,7,4],
             [0,4,0,0,5,0,0,3,6],
             [7,0,3,0,1,8,0,0,0]]

boardsTwoos = [[0,2,0,6,0,8,0,0,0],
               [5,8,0,0,0,9,7,0,0],
               [0,0,0,0,4,0,0,0,0],
               [3,7,0,0,0,0,5,0,0],
               [6,0,0,0,0,0,0,0,4],
               [0,0,8,0,0,0,0,1,3],
               [0,0,0,0,2,0,0,0,0],
               [0,0,9,8,0,0,0,3,6],
               [0,0,0,3,0,6,0,9,0]]


main = do
  putStrLn "First board: "
  printBoard firstBoard 
  print $ checkSudoku firstBoard 
  putStrLn "\nSecond board: "
  printBoard secondBoard 
  print $ checkSudoku secondBoard 
   where
      firstBoard = solveSudoku boardsOne
      secondBoard = solveSudoku boardsTwoos
