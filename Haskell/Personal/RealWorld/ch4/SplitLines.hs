module SplitLines
  (splitLines)
    where

splitLines :: String -> [String]
splitLines [] = []
splitLines cs = 
  let (pre, suf) = break isLineTerminator cs
   in pre : case suf of 
              ('\r' : '\n' : rest) -> splitLines rest
              ('\r' : rest) -> splitLines rest
              ('\n' : rest) -> splitLines rest
              rest          -> splitLines rest
isLineTerminator c = c == '\n' || c == '\r'
