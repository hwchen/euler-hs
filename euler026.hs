-- reciprocal cycles
-- euler 26

-- find value d for d < 1000 which has the longest cycle for 1/d

import Data.List

-- cheat a bit, just add a 0 to remainder for 1/n fractions
gen1Fraction :: Int -> Int -> [Int]
gen1Fraction _ 0 = [0] --actually undefined, but 0 for simplicity
gen1Fraction 0 _ = [0]
gen1Fraction n k = n `div` k : gen1Fraction remainder k
    where remainder = n `rem` k * 10 -- always add 0 for 1/x fractions

-- cuts off all zeros, 
-- trying to reverse. Hard cut at 100
cleanFraction :: Int -> Int -> [Int]
cleanFraction n k = reverse $ take 200 $ dropWhile (==0) $ gen1Fraction n k

-- cycle must be either multiple of 6 or 7. All other ones have short periods.

--finding period - 996 is a long one.
-- take care of at least one repeat in a row.
-- also should take care of finite decimals here.
isPeriod :: Int -> [Int] -> Bool
isPeriod n xs = period' == period'' && period' == period'''
    where period' = take n xs
          period'' = take n $ drop n xs
          period''' = take n $ drop (2*n) xs

-- take returns as much list as possible no error.
findPeriod :: [Int] -> Int
findPeriod [] = 0
findPeriod xs = go 1
    where go n | n > 200 = 0 -- hard cutoff, for testing
               | isPeriod n xs = n 
               | otherwise = go (n+1)


findMaxPeriodTo :: Int -> [(Int,Int)]
findMaxPeriodTo n = zip (map (findPeriod . cleanFraction 1) [1..n]) [1..]

main :: IO()
main = print $ maximum $ findMaxPeriodTo 1000
-- gut was right after seeing pattern of 1/6n giving longer periods as n increased.
-- ah, 100 didn't catch it because I test 3 period to make sure 3 digits in a row
-- doesn't abort.

-- This seems a little like chaotic behavior: complex behavior arising out of simple
-- rules. I wonder how this maps?
