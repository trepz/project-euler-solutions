import Data.List

charToNum :: Char -> Int
charToNum x = case (elemIndex x ['A'..'Z']) of
  Just n  -> n + 1
  Nothing -> 0

answer = 
  sum . 
  zipWith (*) [1..] . 
  map (sum . map charToNum) . 
  sort . 
  lines <$> readFile "names.txt"