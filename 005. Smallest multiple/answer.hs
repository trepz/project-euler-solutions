answer :: Int
answer = smallestDiv 10

smallestDiv :: Int -> Int
smallestDiv n = sm n n 1

sm :: Int -> Int -> Int -> Int
sm n m x
  | n == 1 = m
  | rem (m * x) n == 0 = sm (n - 1) (m * x) 1
  | otherwise = sm n m (x + 1)