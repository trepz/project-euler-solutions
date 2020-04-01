factors :: Int -> Int
factors n = head . dropWhile (\x -> (factors x) < n) . scanl1 (+) $ [1..] where
  mx = round . sqrt . fromIntegral
  factors y = (*2) . length $ filter (\z -> rem y z == 0) [1..(mx y)]

answer = factors 500
