sumOfSquares :: [Int] -> Int
sumOfSquares nums = sum . map (^2) $ nums

squareOfSums :: [Int] -> Int
squareOfSums nums = sum nums ^ 2

diff :: Int -> Int
diff n = abs (a - b) where
  t = [1..n]
  a = sumOfSquares t
  b = squareOfSums t

answer :: Int
answer = diff 100