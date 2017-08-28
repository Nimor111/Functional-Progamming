import Text.Regex.Posix

match' :: String -> String -> Bool
match' str pat = ( str =~ pat ) :: Bool
