-- Gives the days of a given month in a given year
daysOfMonth :: Int -> Int -> [Int]
daysOfMonth month year = [1..n] where
  isLeap = if (rem year 100) == 0 then (rem year 400) == 0 else (rem year 4) == 0
  n | elem month [4, 6, 9, 11] = 30
    | month == 2 && isLeap = 29
    | month == 2 = 28
    | otherwise = 31

-- Given a range of years, generate a list of days of the month
daysOfYears :: [Int] -> [Int]
daysOfYears years = concat [(daysOfMonth month year) | year <- years, month <- [1..12]]

-- First find the starting date of 1901 then zip days of years with with
-- days of week (represented by 1..7 minus the starting date offset) 
-- and count the pairs of 7's and 1's (Sunday and first day of the month)
answer = 
  let (offset, _) = last $ zip (cycle [1..7]) (daysOfYears [1900]) in
  length . 
  filter (== True) $ 
  zipWith (\a b -> [a, b] == [7, 1]) 
          (drop offset $ cycle [1..7]) 
          (daysOfYears [1901..2000])