-- pandigital multiples

-- take a number, multiply it by [1..n]. Concatenate the products
-- if it's pandigital.

-- What is the largest 1-9 pandigital that can be created through this method?

-- bounds: largest pandigital is 987654321
-- smallest pandigital is 123456789
-- smallest list of multipliers is [1,2]
-- largest list of multipliers is [1..5]? 
-- actually, it's [1..6], which gives 9 digits when 3 is the multiplicand.

-- 4 and 6 don't have any pandigitals.
-- I tried to do Max earlier, but introduced errors. Still super fast
-- even concatenating all the lists together.

-- pretty ugly because i keep on moving in and out of lists/strings.

import Data.List
import Utils (intToList, listToInt)

isPan19 :: [Int] -> Bool
isPan19 ns = (length $ nub ns) == 9 && (0 `notElem` ns) && length ns == 9

gen1to :: Int -> [[Int]]
gen1to n = takeWhile (\a -> limit a) $ map (\x -> map (*x) [1..n]) [1..]
    where limit xs = (length $ concat $ map (\y -> intToList y 10) xs)  <= 9 

pan1to :: Int -> [Int]
pan1to n = map listToInt $ filter isPan19 (map (concatMap (\y -> intToList y 10)) (gen1to n)) 

--maxPan1to :: Int -> Int
--maxPan1to n = maximum $ pan1to n

maxPan :: Int
maxPan = maximum $ concatMap pan1to [2..6]
