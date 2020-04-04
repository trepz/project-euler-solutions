import Data.Char (digitToInt)

ones :: [String]
ones = [ "zero", "one", "two", "three", "four", "five"
       , "six", "seven", "eight", "nine", "ten", "eleven"
       , "twelve", "thirteen", "fourteen", "fifteen", "sixteen"
       , "seventeen", "eighteen", "nineteen"
       ]

tens :: [String]
tens = [ "", "", "twenty", "thirty", "forty", "fifty"
       , "sixty", "seventy", "eighty", "ninety"
       ]

numberToList :: Int -> [Int]
numberToList = map digitToInt . show

listToNumber :: [Int] -> Int
listToNumber = read . foldl1 (++) . map show

numberToWords :: Int -> String
numberToWords n
  | n < 20 = ones !! n
  | n < 100 = let [t, o] = (numberToList n) in
    tens !! t ++ if rem n 10 /= 0 then ones !! o else ""
  | n < 1000 = let (h:t) = numberToList n in
    ones !! h ++ "hundred" ++ if all (== 0) t then "" else 
      "and" ++ numberToWords (listToNumber t)
  | otherwise = "onethousand"

answer :: Int
answer = length . foldl1 (++) . map numberToWords $ [1..1000]

    