module InteractWith where

import System.Environment (getArgs)
import SplitLines

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

fixLines :: String -> String
fixLines input = unlines $ splitLines input

main = mainWith myFunction
  where mainWith function = do
        args <- getArgs
        case args of 
          [input, output] -> interactWith function input output
          _ -> putStrLn "error: exactly two arguments needed"
      
myFunction = fixLines
