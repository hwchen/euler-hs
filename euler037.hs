-- euler 37

-- truncatable primes

-- truncating from left to right, and right to left, all are primes.
-- not including 

-- 4 seconds under ghc -O2

import Data.List
import Utils (isPrime, intToList, listToInt)

primeList:: [Int]
primeList = filter isPrime [1..]

truncListR  :: Int -> [Int]
truncListR n = filter (/=0) $map listToInt $ inits $ intToList n 10

truncListL  :: Int -> [Int]
truncListL n = filter (/=0) $map listToInt $ tails $ intToList n 10

truncList :: Int -> [Int]
truncList n = truncListR n ++ truncListL n

isTruncPrime:: Int -> Bool
isTruncPrime n = foldr (\a acc -> a && acc) True $ map isPrime $ truncList n

main :: IO()
main = print $ sum $ take 11 $ filter isTruncPrime $ dropWhile (<10) primeList
