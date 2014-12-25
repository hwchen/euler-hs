-- powerful digit counts
-- euler 063

-- how many n-digit integers exist which also are an nth power?

-- upper bound, at what point does nth power go way beyond n digits?
-- oops, I think I need Integer

-- ah, I didn't understand the upper bound.
-- in x^n, the x always has to be < 10, because if x is 10, x^n will always
-- have greater than n digit count. e.g. 10^2 gives 3 digits, 10^3 gives 4 digits.

-- knowing that, the upper bound could be calculated, but a little complicated.
-- But can see from mucking around that at n=21, x^n starts to diverge from digitCount.

import Utils (intToList)

digitCount :: Integer -> Integer
digitCount n = toInteger $ length $ intToList n 10 

testNForAnswer :: Integer -> [Integer]
testNForAnswer n = filter (\x -> (digitCount (x^n) == n)) [1..100] 

main :: IO()
-- main = print $ map testNForAnswer [1..200]
main = print $ length $ concat $ map testNForAnswer [1..200]
