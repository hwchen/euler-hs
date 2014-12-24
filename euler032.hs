-- pandigital products euler 32

-- sum of the _products_
-- whose mulitplicand/multiplier/product
-- can be written as a 1-9 pandigital

-- what is the limit of product?
-- at least 2 digits for each of multipliers
-- 1 and 2 can never reach 6
-- 2 and 2 can never reach 5
-- 2 and 3 can reach 4
-- so upper bound for product is 4 digits. 9876.

-- also, has to be at least 4 digits. if only three,
-- then multipliers have to be 3 or 4 digits, not possible

-- going from generating combinations to generating products went
-- from about 6 seconds to .028 seconds.

-- whoops, mistake in isPanDigital

-- also, 4 and 1 can reach 4. Missed that in the logic above.
-- with correct bins, takes .625s

import Data.List
import Utils (intToList)

isPanDigital19 :: [Int] -> Bool
isPanDigital19 ns = length (nub ns) == 9 && (0 `notElem` ns) && length ns == 9

products :: [(Int, Int, Int)]
products = [(a, b, a*b) 
                     | b <- [100..9999]
                     , a <- [1..99]
                     , a * b <= 99999]

prodID :: (Int, Int, Int) -> [Int]
prodID (a,b,c) = concat [intToList a 10, intToList b 10, intToList c 10]

panProducts :: [(Int, Int, Int)]
panProducts = filter (\ns -> isPanDigital19 $ prodID ns) products

third :: (Int, Int, Int) -> Int
third (_, _, x) = x

main :: IO()
main = print $ sum $ nub $ map third panProducts 
