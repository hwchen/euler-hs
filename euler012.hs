-- Euler 12
-- divisible triangle numbers.

-- Triangle number n is sum of all numbers up to n.
-- what is first triangle number to have over 500 divisors.

triNumList :: Integral a => [a]
triNumList = map triNum [1..]

triNum :: Integral a => a -> a
triNum 0 = 0
triNum n = n + triNum (n-1)

-- I could go to square root, then generate the other half of divisors.
-- no! only need to know length, then can (length-2) + 2 to get real
-- length
-- This is way faster than original way (filter up to /2)
-- 30 seconds with ghci -O2, starting from 500 (but incorrect, it's the 
-- 500th trinum.
-- But ends up the same either way. 30 seconds even from [1..]
-- whoops, didn't subsitute divListLength. Now time goes down to
-- 2 seconds?

divisorList :: Integral a => a -> [a]
divisorList n = go 2
    where go x 
            | x * x > n = []
            | n `rem` x == 0 = x : go (x+1)
            | otherwise = go (x+1)

divListLength :: [a] -> Int 
divListLength xs = 2*(length xs) + 2

-- need to use Int because length is Int
triNumOverXDivisors :: Int -> Int 
triNumOverXDivisors x = 
    head $ dropWhile (\y -> (divListLength $ divisorList y) < x) triNumList

main = print $ triNumOverXDivisors 500

