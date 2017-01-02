import Data.List

uniqueEls :: Eq a => [a] -> Int
uniqueEls = length . nub

-- Data.List methods
-- intersperse -> 0 [1,2,3] -> [1,0,2,0,3,0]
-- intercalate -> [0,0,0] [[1,2,3],[4,5,6]] -> [1,2,3,0,0,0,4,5,6]
-- transpose -> self explanatory
-- foldl' and foldl1' -> stricter versions, not lazy, they compute
-- intermediate values
-- concat -> [[1,2,3],[4,5,6]] -> [1,2,3,4,5,6]
-- concatMap -> concat + map
-- and $ map on list -> True if all els in list are True
-- or $ map on list -> True if any of the els in list is True
-- any / all -> equivalent of or $ map and and $ map
-- iterate -> returns stuff in form of infiite list
-- ex -> iterate (*2) 1 -> [2,4,8,16,32,64,...]
-- ex -> iterate (++"haha") "haha" -> ["haha", "hahahaha", "hahahahahaha"]
-- splitAt -> split at given element
-- takeWhile -> takes elements while predicate is True
-- dropWhile -> drops elements while predicate is True
-- span predicate -> returns pair of lists -> first one is takeWhile with
-- pred, second is dropWhile with pred
-- break pred == span ( not . pred ) -> breaks when pred is first true
-- break (==4) [1,2,3,4,5,6,7] -> [[1,2,3], [4,5,6,7]]
-- sort -> sorts a list, must implement Ord
-- group -> [1,1,1,1,2,2,3,3] -> [[1,1,1,1],[2,2],[3,3]]
-- inits and tails -> applies init and tail to list until it's empty
-- inits "woot" -> ["", "w", "wo", "woo", "woot"]

-- search list for sublist
search :: Eq a => [a] -> [a] -> Bool
search needle haystack =
  let len = length needle
   in foldl (\acc x -> ((take len haystack == needle) || acc ))
      False (tails haystack)

-- isInfixOf - same effect as search
-- isPrefix, SuffixOf -> searches at beg and end of list
-- elem, notElem
-- partition -> groups els by predicate
-- partition (>3) [1,2,3,4,5,6,7] -> [[4,5,6,7], [1,2,3]]
-- find pred -> returns first el satisfies pred wrapped in Maybe value
-- Maybe a = Just a | Nothing
-- elemIndex -> returns Maybe index of element
-- elemIndices -> indexes of each occurrence of element, doesn't need Maybe
-- can return empty list == Nothing
-- findIndex -> Maybe finds index that satisfies pred ( returns Nothing if 
-- error, maybe that's the point of Maybe )
-- zip and zipWith -> zip3, zip4 - to 7, pretty cool
-- lines -> returns every line of string given as arg
-- unlines -> opposite of lines
-- words, unwords -> split string into words, join words into string
-- nub -> removes duplicates
-- delete -> dels first occurrence of el in list
-- \\ -> set difference ( разлика на множества ) -> lists, obviously
-- union -> set union, obvs
-- insersect -> set intersect
-- insert -> kinda strange, inserts el into list, keeps sort of list
-- insert 4 [3,5,1,2,8,2] -> [3,4,5,1,2,8,2]
-- generic versions of length, take, drop, splitAt, !!, replicate -> 
-- can return any type that implements Num typeclass
-- generic versions of nub, delete, union, intersect, group
-- nubBy, groupBy, etc.
-- equality funcs for the by versions -> on from Data.Function
-- groupBy ( (==) `on` (> 0) ) values -> pretty damn fucking cool
-- and of course -> sortBy, insertBy, maximumBy, minimuBy 
-- example -> sortBy (compare `on` length ) xxs -> sorts list of lists
-- by length of lists -> sounds like english!

main = do
  print(uniqueEls [1,2,3,4,4,5])
  print(uniqueEls [1,1,1,1,1,1])
