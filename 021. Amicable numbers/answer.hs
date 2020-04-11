import Data.List

divisors :: Int -> [Int]
divisors n = 1 : filter ((==0) . rem n) [2 .. n `div` 2]

indexOf :: [a] -> Int -> Maybe a
indexOf list index
  | index > (length list) - 1 = Nothing
  | otherwise = Just (list !! index)

pairs :: [Int] -> [(Maybe Int, Maybe Int, Maybe Int)]
pairs list = map pair list where
  p n = indexOf list (n - 1)
  pair n = (Just n, p n, p n >>= p)

isAmicable :: (Eq a) => (a, a, a) -> Bool
isAmicable (a, b, c) = a == c && a /= b

answer = 
  sum .
  nub . 
  map (\(a, _, _) -> maybe 0 id a) . 
  filter isAmicable . 
  pairs . 
  map (sum . divisors) $ [1..10000]
