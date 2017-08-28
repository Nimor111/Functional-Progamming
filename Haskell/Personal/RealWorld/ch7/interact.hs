import Data.Char (toUpper)

main :: IO ()
main = interact $ (++) "Your data, in uppercase, is: \n\n" . map toUpper

-- return wraps a value in monad
isGreen :: IO Bool
isGreen = do 
  putStrLn "Is green your favourite color?"
  inpStr <- getLine
  return ( ( toUpper . head $ inpStr) == 'Y' )
