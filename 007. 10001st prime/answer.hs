primes :: [Int]
primes = 2 : 3 : filter (isPrime primes) [5, 7..] where
  isPrime (x:xs) n
    | x * x > n = True
    | otherwise = rem n x /= 0 && isPrime xs n

answer = primes !! 10000