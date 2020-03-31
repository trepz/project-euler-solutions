primes :: [Int]
primes = 2 : 3 : filter (isPrime primes) [5, 7..] where
  isPrime (x:xs) n
    | x * x > n = True
    | otherwise = rem n x /= 0 && isPrime xs n


primeFactors :: [Int] -> Int -> [Int]
primeFactors factors 1 = factors
primeFactors factors n = primeFactors (factors ++ [lowest]) nextFactor where
  lowest = head $ dropWhile (\x -> rem n x /= 0) primes
  nextFactor = div n lowest


answer :: Int
answer = maximum $ primeFactors [] 600851475143