factors :: Int -> Int
factors y = (*2) . length $ filter (\z -> rem y z == 0) [1..(mx y)] where
  mx = round . sqrt . fromIntegral

divs :: Int -> Int
divs n = head . dropWhile (\x -> (factors x) < n) . scanl1 (+) $ [1..]

answer :: Int
answer = divs 500
