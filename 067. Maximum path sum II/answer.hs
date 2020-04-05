import System.IO

triangleToDigits :: String -> [[Int]]
triangleToDigits = (map read <$>) . map words . lines

maxPathSum :: [[Int]] -> [Int]
maxPathSum [x] = x
maxPathSum (x:xs) = 
  imap (\i v -> v + (maxNext i)) 0 x where 
    t = maxPathSum xs
    maxNext index = max (t !! index) (t !! (index + 1))

-- map with index
imap :: (Num c) => (c -> a -> b) -> c -> [a] -> [b]
imap f _ [] = []
imap f i [x] = [f i x]
imap f i (x:xs) = (f i x) : imap f (i + 1) xs

answer = head .  maxPathSum . triangleToDigits <$> readFile "triangle.txt"