combinations :: Int -> [Int] -> Int
combinations target [] = 0
combinations target (x:xs) = 
  n + (sum $ map (\y -> combinations (target - y) xs) $ 0 : [x,(x * 2)..(target - x)]) where
    n = if rem target x == 0 then 1 else 0

answer = combinations 200 [1, 2, 5, 10, 20, 50, 100, 200] 