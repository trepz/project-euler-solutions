collatz :: Int -> [Int]
collatz n 
  | n == 1 = n : []
  | even n = n : collatz (div n 2)
  | odd n = n : collatz (3 * n + 1)

answer = 
  let range = [1..999999] in
    snd . maximum $ zip (map (length . collatz) range) range