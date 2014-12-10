-- Euler 34
-- find numbers where sum of digits! = number
-- odd, can only find 145. so brute force is not enough

import Utils (intToList, fac)

digitsFactorialSum :: Integral a => a -> a
digitsFactorialSum x = sum $ map fac $ intToList x 10

isDigitsFactorialSumEqual :: Integral a => a -> Bool
isDigitsFactorialSumEqual 1 = False
isDigitsFactorialSumEqual 2 = False
isDigitsFactorialSumEqual x = x == digitsFactorialSum x 

sumDigitsFactorial :: Integral a => a
sumDigitsFactorial = sum $ filter isDigitsFactorialSumEqual [100000000..1000000000]

digitsFactorial :: Integral a => [a]
digitsFactorial = filter isDigitsFactorialSumEqual [100000000.. 1000000000]

main = print $ digitsFactorial
