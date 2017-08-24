class BasicEq a where
  isEqual :: a -> a -> Bool
  isEqual x y = not (isNotEqual x y)

  isNotEqual :: a -> a -> Bool
  isNotEqual x y = not (isEqual x y)

instance BasicEq Bool where
  isEqual True True   = True
  isEqual False False = True
  isEqual _     _     = False

data Color = Red | Green | Blue deriving Show

stripWhitespace :: String -> String
stripWhitespace = filter (/= ' ')

-- same as instance compiler would automatically derive for us
instance Read Color where
  readsPrec _ value =
    tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
      where
        tryParse [] = []
        tryParse ((attempt, result):xs) =
          if take (length attempt) sValue == attempt
             then [(result, drop (length attempt) sValue)]
          else
            tryParse xs
        sValue = stripWhitespace value
