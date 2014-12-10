-- Euler 34
-- find numbers where sum of digits! = number
-- odd, can only find 145. so brute force is not enough
-- oops, I defined factorial incorrectly (with 0! = 0)

-- to find upper bound: 9! is max sum for one digit
-- n*9! for n digits
-- where do n*9! and 10^n-1 meet? around n = 7.5

import Utils (intToList, fac)

digitsFactorialSum :: Integral a => a -> a
digitsFactorialSum x = sum $ map fac $ intToList x 10

sumDigitsFactorial :: Integral a => a -> a
sumDigitsFactorial x = sum $ filter (\n -> n == digitsFactorialSum n) [3..x]

main = print $ sumDigitsFactorial 10000000
