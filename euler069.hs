-- totient maximum
-- two numbers are relatively prime if their only common divisor is 1.
-- totient function is:
-- n divided by number of numbers less than n that are relatively prime.

--totient function is phi

-- Speedup: instead of using Set. When filtering, to check if isRelativePrime, stop if a
-- common divisor is found.

import qualified Data.Set as S
import Utils (divisorsList)

isRelativePrime :: Int -> Int -> Bool
isRelativePrime n k = go 2
    where go x | x > n = True 
               | (n `rem` x  == 0) && (k `rem` x == 0) = False
               | otherwise = go (x+1)

-- phiList of set of divisors
phiList' :: Int -> [Int]
phiList' 0 = []
phiList' n = map fst $ filter (\(_,xs) -> xs `S.intersection` divisorsN == S.fromList [1]) $ zip [1..] divisorsOfDivisors
    where divisorsOfDivisors = map (S.fromList . divisorsList) [1..n]
          divisorsN = S.fromList $ divisorsList n

-- phiList' ends up being much faster at large numbers.
-- maybe even a couple of orders of magnitude.
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
-- 92400 is 4.8125
testList :: [Int]
testList = filter (\x -> x `rem` 92400 == 0) [1..1000000]

-- I think i need to also memoize phiList'

main :: IO()
main = print $ maximum $ map (\n -> (phiRatio n, n)) testList
