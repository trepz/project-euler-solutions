factors :: Int -> Int
factors n = head . dropWhile ((< n) . factors) . scanl1 (+) $ [1..] where
  mx = round . sqrt . fromIntegral
  factors y = (*2) . length $ filter ((== 0) . (rem y)) [1..(mx y)]

answer = factors 500
