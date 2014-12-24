--d euler 23
-- non abundant sums

import Data.List
import qualified Data.Set as Set
import Utils (divisorsList)

properDivisorsList :: Integral a => a -> [a]
properDivisorsList 0 = []
properDivisorsList n = init $ divisorsList n

isAbundant :: Integral a => a -> Bool
isAbundant n
    | (sum $ properDivisorsList n) > n = True
    | otherwise = False

abListTo :: Integral a => a -> [a]
abListTo n = filter isAbundant [1.. n]

sumTwoAbListTo :: Integral a => a -> [a]
sumTwoAbListTo n = [a+b | 
                           a <- abListTo n, 
                           b <- takeWhile (<=a) $ abListTo n,
                           a+b <= n]


-- sum TwoAbListTo works.
-- problem is with the filter? Too much memory?
-- maybe `notElem` is too expensive. So filter pred...

--notSumTwoAbListTo :: Integral a => a -> [a]
--notSumTwoAbListTo n = filter isSumTwoAb [1..n]

--well, using sets is much faster. I think thse slowness now comes from having
-- to call divisorList so many times.

main :: IO()
main = do
    print $ sum $ filter (\n -> n `Set.notMember` (Set.fromList $ sumTwoAbListTo 28123)) [1..28123]
    print $ (sum [1..28123]) - (sum $ filter (\n -> n `Set.member` (Set.fromList $ sumTwoAbListTo 28123)) [1..28123])
