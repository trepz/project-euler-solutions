import Data.List

divisors :: Int -> [Int]
divisors n = 1 : filter ((==0) . rem n) [2 .. n `div` 2]

isAmicable :: [Int] -> Int -> Bool
isAmicable list n
  | x > ln = False
  | list !! x > ln = False
  | list !! x == n = False
  | list !! ((list !! x) - 1) == n = True
  | otherwise = False
  where 
    ln = length list
    x = (n - 1)

answer =
  let 
    divs = map (sum . divisors) $ [1..10000]
  in 
    sum . nub . filter (isAmicable divs) $ divs