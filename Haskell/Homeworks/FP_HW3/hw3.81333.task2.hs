type Book = (String, Int)
type Library = [Book]

findNumOfOccurences :: [Int]-> Int -> Int 
findNumOfOccurences xs x = sum ( map (\ y -> if y == x then 1 else 0) xs)

findUniques :: Library -> [String]
findUniques lib = [fst name | name <- lib, findNumOfOccurences
                  (map snd lib) (snd name) == 1] 

longestTitleYear :: Library -> Int 
longestTitleYear lib = snd (head max')
  where max' = [el | el <- lib, length (fst el) == 
                maximum (map (length . fst) lib)] 
