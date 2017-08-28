import System.IO
import Data.Char (toUpper)

main :: IO ()
main = do
  inh <- openFile "input.txt" ReadMode
  outh <- openFile "input.txt" WriteMode
  inputStr <- hGetContents inh
  let result = processData inputStr
  hPutStr outh result
  hClose inh
  hClose outh

processData :: String -> String
processData = map toUpper
