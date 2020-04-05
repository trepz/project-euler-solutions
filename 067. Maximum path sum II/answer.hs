triangleToDigits :: String -> [[Int]]
triangleToDigits = reverse . (map read <$>) . map words . lines

maxPathSum :: [[Int]] -> [Int]
maxPathSum [x] = x
maxPathSum (y:x:xs) = 
  let m = zipWith (+) (largestNeighbour y) x
  in maxPathSum (m:xs)

largestNeighbour :: [Int] -> [Int]
largestNeighbour [x] = []
largestNeighbour (y:x:xs) = (max x y) : (largestNeighbour (x:xs))

answer = head . maxPathSum . triangleToDigits <$> readFile "triangle.txt"