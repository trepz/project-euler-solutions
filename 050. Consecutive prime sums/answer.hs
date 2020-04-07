primes :: [Int]
primes = 2 : 3 : filter (isPrime primes) [5, 7..] where
  isPrime (x:xs) n
    | x * x > n = True
    | otherwise = rem n x /= 0 && isPrime xs n

find :: [Int] -> Int -> Bool
find [] n = False
find (x:xs) n
  | x == n = True
  | x < n = find xs n
  | otherwise = False

primeSumFrom :: Int -> Int -> (Int, Int)
primeSumFrom limit n = (length p, if (length p > 0) then head p else 0) where 
  p = dropWhile (not . (find primes)) . reverse . takeWhile (< limit) . scanl1 (+) . drop n $ primes

answer = snd . maximum . map (primeSumFrom 1000000) $ [0..100]
  