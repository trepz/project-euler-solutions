answer :: Int
answer = product . head $ list
  where list = [[a, b, 1000 - a - b]
               | a <- [1..1000]
               , b <- [a..1000]
               , a ^ 2 + b ^ 2 == (1000 - a - b) ^ 2
               ]