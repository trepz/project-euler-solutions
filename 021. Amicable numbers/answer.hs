import Data.List

divisors :: Int -> [Int]
divisors n = 1 : filter ((==0) . rem n) [2 .. n `div` 2]

pairs :: [Int] -> [(Maybe Int, Maybe Int, Maybe Int)]
pairs list = map pair list where
  p n = if ((n - 1) > (length list)) then Nothing else Just (list !! (n - 1))
  pair n = (Just n, p n, p n >>= p)

answer = sum
       . nub
       . map (\(a, _, _) -> maybe 0 id a)
       . filter (\(a, b, c) -> a == c && a /= b)
       . pairs 
       . map (sum . divisors) 
       $ [1..10000]
