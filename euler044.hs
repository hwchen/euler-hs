-- pentagonal numbers euler 44
-- pentagonal pair of numbers whose sum and difference 
-- are also pentagonal, and their distance is minimized.

-- I found one pair, but it took 10 minutes!
-- both numbers were between 1 million and 10 million

-- ah, the new trick: for second part of combination, takeWHile less 
-- than first half. Well, not that much better. Try adding set.

-- Took less than a second when I switched to using Set. Having a good
-- set makes even more difference than using a takeWhile in isPNum to limi
-- the length of the list. Set is a binary search tree?
-- Doesn't even matter how many pNums I precompute, doesn't really change
-- runtime.

import qualified Data.Set as Set

pNums :: [Int]
pNums = [n*(3*n - 1) `div` 2 | n <- [1..1000000]]

isPNum :: Int -> Bool
isPNum n = n `Set.member` Set.fromList pNums


pNumPairsFromTo :: Int -> Int -> [(Int,Int)]
pNumPairsFromTo n0 n = [(a,b) | a <- pNums', b <- takeWhile (<a) pNums', a /= b, isPNum (a+b), isPNum (abs (b-a))]
    where pNums' = takeWhile (<= n) $ dropWhile (<= n0) pNums

main :: IO()
main = print $ pNumPairsFromTo 1 10000000
