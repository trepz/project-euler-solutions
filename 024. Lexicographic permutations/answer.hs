import Data.List

sortedPerms :: [Int] -> [Int]
sortedPerms = sort . map (read . concat . (map show)) . permutations

answer = sortedPerms [0..9] !! (10 ^ 6 - 1)