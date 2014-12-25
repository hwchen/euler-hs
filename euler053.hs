-- combinatoric selections, euler 53

-- combination is n C r.
-- defined as n!/ (r!(n-r!))
-- jactorial, 0! = 1

-- 23 is the first n where a value of C exceeds 1 million
-- for combinations of n C r, how many with n<=100, are greater than 1 million

import Utils (fac)

nCr :: (Integer,Integer) -> Integer
nCr (n, r) = fac n `div` (fac r * (fac (n-r)))

nrCombos :: [(Integer,Integer)]
nrCombos = [(n,r) | n <- [23..100], r <- takeWhile (<n) [1..100]]

nrValues :: [Integer]
nrValues = map nCr nrCombos

main :: IO()
main = print $ length $ filter (>1000000) nrValues
