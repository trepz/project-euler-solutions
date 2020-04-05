-- Read the text file in to a list of lists, reversed so we can start
-- from the bottom and work up.
triangleToDigits :: String -> [[Int]]
triangleToDigits = reverse . (map read <$>) . map words . lines

-- The max path sum is simply the largest pairs of the current line,
-- added to the values of the next line, repeated until we get to a
-- single entry.
maxPathSum :: [[Int]] -> [Int]
maxPathSum [x] = x
maxPathSum (y:x:xs) = 
  let m = zipWith (+) (largestNeighbour y) x
  in maxPathSum (m:xs)

-- Find the largest neighbouring pairs in a list, e.g./
-- [2,4,3] -> [4,4]
-- because 4 is the largest neighbour of [2,4] and [4,3]
largestNeighbour :: [Int] -> [Int]
largestNeighbour [x] = []
largestNeighbour (y:x:xs) = (max x y) : (largestNeighbour (x:xs))


answer = head . maxPathSum . triangleToDigits <$> readFile "triangle.txt"