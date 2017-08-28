import Data.Char (toLower)

main :: IO ()
main = do
  input <- readFile "input.txt"
  writeFile "output.txt" $ map toLower input
