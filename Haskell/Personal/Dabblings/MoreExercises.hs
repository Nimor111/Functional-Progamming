import           Data.Char       (isSpace, toUpper)
import           Data.List       (intercalate)
import           Data.List.Split (splitOn)

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf (c:cs) ss
  | c `elem` ss = isSubsequenceOf cs ss
  | otherwise = False

capitalizeWords :: String -> [(String, String)]
capitalizeWords str = words str >>= (\w -> [(w, capitalizeWord w)])

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (c:word)
  | isSpace c = ' ' : capitalizeWord word
  | otherwise = toUpper c : word

capitalizeParagraph :: String -> String
capitalizeParagraph = intercalate "." . map capitalizeWord . splitOn "."
