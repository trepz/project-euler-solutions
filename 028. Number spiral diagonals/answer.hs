-- Chunks a list using another list
chunk :: [Int] -> [a] -> [[a]]
chunk _ [] = []
chunk [] a = [a]
chunk (x:xs) a = (take x a) : chunk xs (drop x a)

-- Spiral pattern to chunk by
spiral n = (take 4 (repeat n)) ++ spiral (n + 2)

-- Solve for spiral of n x n
solve :: Int -> Int
solve n = (+ 1) . sum . map last . chunk (spiral 2) $ [2..(n ^ 2)]

answer = solve 1001