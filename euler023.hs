--d euler 23
-- non abundant sums
import Data.List
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
                           a <- abListTo (n `div` 2), 
                           b <- abListTo n, 
                           a+b <= n]


-- sum TwoAbListTo works.
-- problem is with the filter? Too much memory?
-- maybe `notElem` is too expensive. So filter pred...

--notSumTwoAbListTo :: Integral a => a -> [a]
--notSumTwoAbListTo n = filter isSumTwoAb [1..n]

main :: IO()
main = print $ length $ sort $ sumTwoAbListTo 28123
