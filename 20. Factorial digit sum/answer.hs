import Data.Char (digitToInt)

answer = sum . map digitToInt . show . product $ [1..100]