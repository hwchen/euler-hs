-- totient maximum
-- two numbers are relatively prime if their only common divisor is 1.
-- totient function is:
-- n divided by number of numbers less than n that are relatively prime.

--totient function is phi

-- Speedup: instead of using Set. When filtering, to check if isRelativePrime, stop if a
-- common divisor is found.
{-# LANGUAGE BangPatterns #-}

import qualified Data.Set as S
import Utils (divisorsList)

--preload divisorsOfDivisors
divisorsOfDivisors :: [[Int]]
divisorsOfDivisors = map divisorsList [1..1000000]


-- phiList of set of divisors
phiList' :: Int -> [Int]
phiList' 0 = []
phiList' n = map fst $ filter (\(_,xs) -> xs `S.intersection` divisorsN == S.fromList [1]) $ zip [1..] divisorsOfDivisorsToN
    where divisorsOfDivisorsToN = map S.fromList $! take n divisorsOfDivisors 
          divisorsN = S.fromList $! divisorsList n --take from preloaded?

-- phiList' ends up being much faster at large numbers.
-- maybe even a couple of orders of magnitude.
isRelativePrime :: Int -> Int -> Bool
isRelativePrime n k = go 2
    where go x | x > n = True 
               | (n `rem` x  == 0) && (k `rem` x == 0) = False
               | otherwise = go (x+1)

phiList :: Int -> [Int]
phiList n = filter (flip isRelativePrime n) [1..n]

phiRatio :: Int -> Float
phiRatio n = fromIntegral n / fromIntegral (length $ phiList' n)


-- Manual bootstrapping. (and discovery)
-- also, it's probably a number divisible by 2 and 3 and 10..
-- 840 is 4.375
-- 9240 is 4.8125 
-- what's the greatest common divisor? looks like 840
-- 840 might be enough space in list. No, so move onto 9240
-- 92400 is 4.8125, in 
-- 924000 is 4.8125, in 4 min 40 s with bang patterns (did they prevent memory leak?

-- divisorlist at 1 million takes too long. How to speed up? can't really memoize.

-- usig multiples of 9240, found 960960, ratio 5.2135415, 50 minutes! And still not the right answer!

--Round 2, preloading divisorsOfDiviors. But with no testlist filter?
-- 1000 is 840, 1 second
-- 100,000 is 40 seconds with testList filter 840 

testList :: [Int]
testList = takeWhile (< 100000) $ [30,60..]
-- changed to multiples of 6, does this really make it much faster? Weird, with 
-- same filter if multiple of 140, it's faster than
-- having a list [140,280] or [1,2..].
-- < 10000, 9240, .6s
-- < 100000, 60060, 1m
-- < 1000000 (doesn't take 10 times as long, what's the bottleneck?)
-- it's not just finding the divisors, it's creating lists of divisorslists for every number.
-- also, creating sets each time is consuming?

-- I think i need to also memoize phiList'

main :: IO()
main = print $ maximum $ map (\n -> (phiRatio n, n)) testList 
