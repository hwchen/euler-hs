--d euler 23
-- non abundant sums
import Utils (divisorsList)

properDivisorsList :: Integral a => a -> [a]
properDivisorsList 0 = []
properDivisorsList n = init $ divisorsList n

isAbundant :: Integral a => a -> Bool
isAbundant n
    | (sum $ properDivisorsList n) > n = True
    | otherwise = False

isSumOfTwoAbundant :: Integral a => a -> Bool
isSumOfTwoAbundant n = undefined

abundantListTo n = filter isAbundant [1..n]

-- can't check permutations, it's too slow,
notSumTwoAbundantListTo n = 
    filter 
    (\x -> x `notElem` 
    [a+ b| a <- abundantListTo (x `div` 2), b <- take (x `div` 2) $ reverse (abundantListTo x)]) 
    [1..n]

main = print $ sum $ notSumTwoAbundantListTo 28123
