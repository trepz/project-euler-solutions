answer :: Int
answer = maximum [a * b | a <- [100..999], b <- [100..999], isPalindrome $ a * b]

isPalindrome :: Int -> Bool
isPalindrome n = n == revN where
  revN = read . reverse . show $ n :: Int