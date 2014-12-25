-- Euler 49
-- find rising arithmetic sequence of 3 4-digit primes,
-- where each prime is a permutation of the other.

-- find list of all 4-digit primes. 

import Data.List
import Utils (isPrime, intToList, listToInt)

primeList4Digit' :: [Int]
primeList4Digit' = filter isPrime [1000..9999]

primePermutations' :: Int -> [Int]
primePermutations' n =  sort $ filter isPrime $ map listToInt $ permutations $ intToList n 10

-- this could be a fold?
evenlySpaced' :: [Int] -> Bool
evenlySpaced' [] = False
evenlySpaced' [x] = False
evenlySpaced' xs
    | (length $ nub $ listDiff' xs) == 1 = True
    | otherwise = False

listDiff' :: [Int] -> [Int]
listDiff' [] = []
listDiff' xs = zipWith (-) xs $ tail xs


-- now to take all combinations of 3 for each primePermutation and 
-- check for evenlyspaced.

-- generate triples first, then check if they're evenly space, then check if they're permutations.

-- taking triples from 4000 choices is a lot. How to narrow down? Oops, take from prime list
-- 6 seconds. Probably took me 10 minutes to write it this time...
-- I spent so much time before...

triples :: [(Int, Int, Int)]
triples = [(a,b,c) 
          | c <- primeList4Digit' 
          , b <- takeWhile (< c) primeList4Digit' 
          , a <- takeWhile (<b) primeList4Digit' 
          , c-b == b-a]

arePerm :: (Int, Int, Int) -> Bool
arePerm (a,b,c) = a' == b' && b' == c'
    where a' = sort $ intToList a 10
          b' = sort $ intToList b 10
          c' = sort $ intToList c 10

answer :: [(Int, Int, Int)]
answer = filter arePerm triples

pprintTriple :: (Int,Int,Int) -> String
pprintTriple (a,b,c) = show a ++ show b ++ show c 

main :: IO()
main = print $ pprintTriple $ last answer
