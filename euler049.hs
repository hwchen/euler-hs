-- Euler 49
-- find rising arithmetic sequence of 3 4-digit primes,
-- where each prime is a permutation of the other.

-- find list of all 4-digit primes. 

import Data.List
import Utils (isPrime, intToList, listToInt)

primeList4Digit :: [Int]
primeList4Digit = filter isPrime [1000..9999]

primePermutations :: Int -> [Int]
primePermutations n =  sort $ filter isPrime $ map listToInt $ permutations $ intToList n 10

-- this could be a fold?
evenlySpaced :: [Int] -> Bool
evenlySpaced [] = False
evenlySpaced [x] = False
evenlySpaced xs
    | (length $ nub $ listDiff xs) == 1 = True
    | otherwise = False

listDiff :: [Int] -> [Int]
listDiff [] = []
listDiff xs = zipWith (-) xs $ tail xs


-- now to take all combinations of 3 for each primePermutation and 
-- check for evenlyspaced.


