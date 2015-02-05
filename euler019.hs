-- euler 19 
-- counting sundays

--January 1, 1900 was a Monday

-- January: 31
-- February: 28
-- March: 31
-- April: 30
-- May: 31
-- June: 30
-- July: 31
-- August: 31
-- September: 30
-- October: 31
-- November: 30
-- December: 31

-- leap years on any year divisble by 4, unless divisible by 400

-- how many sundays fell on the 1st of the month between
-- January 1 1901 and December 31 2000?

-- find all sundays in date range, then intersect that with the 1st
-- of eac month (iterate?)

--Sundays between January 1, 1901 and December 31, 2000
-- jan 1, 1901 is a Tuesday
-- total days is 36525 
-- Sunday is True
sundays :: [Bool]
sundays = take 36525 $
          replicate 5 False ++ 
          concatMap (\_ -> [True] ++ replicate 6 False) [1..]

-- mapping years to the total days:
yearList :: [Int]
yearList = [1901, 1902.. 2000]

yearToDays :: Int -> Int
yearToDays n
    | n `rem` 400 == 0 = 366 -- div by 400 = 366
    | n `rem` 100 == 0 = 365 --remaining centureis, are 365
    | n `rem` 4   == 0 = 366 -- div by 4 = 365
    | otherwise = 365 

yearsInDays :: [Int]
yearsInDays = map yearToDays yearList

-- map months, then overlay sundays

yearToFirsts :: Int -> [Bool]
yearToFirsts n
    | n `rem` 400 == 0 = leapYear
    | n `rem` 100 == 0 = normYear
    | n `rem` 4   == 0 = leapYear
    | otherwise = normYear
    where leapYear =   [True] ++ replicate 30 False -- January
                    ++ [True] ++ replicate 28 False -- Feb
                    ++ [True] ++ replicate 30 False -- March
                    ++ [True] ++ replicate 29 False -- Apr
                    ++ [True] ++ replicate 30 False -- May
                    ++ [True] ++ replicate 29 False -- June
                    ++ [True] ++ replicate 30 False -- Jul
                    ++ [True] ++ replicate 30 False -- Aug
                    ++ [True] ++ replicate 29 False -- Sept
                    ++ [True] ++ replicate 30 False -- Oct
                    ++ [True] ++ replicate 29 False -- Nov 
                    ++ [True] ++ replicate 30 False -- Dec
          normYear =   [True] ++ replicate 30 False -- January
                    ++ [True] ++ replicate 27 False -- Feb
                    ++ [True] ++ replicate 30 False -- March
                    ++ [True] ++ replicate 29 False -- Apr
                    ++ [True] ++ replicate 30 False -- May
                    ++ [True] ++ replicate 29 False -- June
                    ++ [True] ++ replicate 30 False -- Jul
                    ++ [True] ++ replicate 30 False -- Aug
                    ++ [True] ++ replicate 29 False -- Sept
                    ++ [True] ++ replicate 30 False -- Oct
                    ++ [True] ++ replicate 29 False -- Nov 
                    ++ [True] ++ replicate 30 False -- Dec

yearsToFirsts :: [Bool]
yearsToFirsts = concatMap yearToFirsts yearList

intersectSunFirst :: [Bool]
intersectSunFirst = zipWith (\a b -> a && b) sundays yearsToFirsts

sumIntersectSunFirst :: Int
sumIntersectSunFirst = length $ filter (==True) intersectSunFirst

main :: IO()
main = print sumIntersectSunFirst
-- worked on the first try! Awesome!
