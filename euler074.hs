-- euler 74
-- digit factorial chains.


-- I need to figure out how to memoize, then return.


-- 3 loops: 169, 871, 872

-- find non-repeating chain (eliminate the first member that repeats)
-- chain starting with 69 has 5 terms.

-- with starting number under one million, how many chains of exactly 60

-- simple way:: end chain if a number is in any of 4 chains.
-- then afterwards, if the end of the chain is a certain number,
-- add to length of count

-- recalcultaing the last in the chain adds a *n to O().
{-# LANGUAGE BangPatterns #-}

import qualified Data.Set as S
import Utils (intToList)

fac' 0 = 1
fac' 1 = 1
fac' 2 = 2
fac' 3 = 6
fac' 4 = 24
fac' 5 = 120
fac' 6 = 720
fac' 7 = 5040
fac' 8 = 40320
fac' 9 = 362880

--this is fast, .72s for mapping to [1..1000000]
digitFac :: Int -> Int
digitFac = sum . map fac' . flip intToList 10

dfChainLoops = S.fromList [1, 2, 145, 169, 363601, 1454, 871, 45361, 872, 45362]

-- stops at first element of a known loop.

-- this is the big slowdown. (out of memory)
-- ah, I need to memoize

digitFacChain :: Int -> [Int]
digitFacChain n = case chainSpan of
    (xs, []) -> xs 
    (xs, ys) -> xs ++ [head ys]
    where chainSpan = span (`S.notMember` dfChainLoops) $! iterate digitFac n


dfChainLength :: Int -> Int
dfChainLength n
    | chainLength == 0 = 0
    | chainEnd == 145 = chainLength
    | chainEnd == 169 = chainLength + 2
    | chainEnd == 363601 = chainLength + 2
    | chainEnd == 1454 = chainLength + 2
    | chainEnd == 871 = chainLength + 1
    | chainEnd == 45361 = chainLength + 1
    | chainEnd == 871 = chainLength + 1
    | chainEnd == 45362 = chainLength + 1
    | otherwise = chainLength
    where chainEnd = last $! digitFacChain n
          chainLength = length $! digitFacChain n

chainLength60 :: [Int]
chainLength60 = filter (==60) $ map dfChainLength [1..100000]

-- new solution: if I need to memoize, I need to combine
-- creating fac chain and length at once. And create a more 
-- complicated recursive function.

-- why does even iterChainLengthTo run out of memory?
-- different algorithm?
-- where can I get a true loop? let me try in rust... later.
iterChainLengthTo :: Int -> Int
iterChainLengthTo x = loop 0 1
    where loop count n | n == x+1 = count
                       | (length $!(digitFacChain n)) == 60 = loop (count + 1) (n+1)
                       | otherwise = loop count (n+1)

main :: IO()
--main = print $ length $! filter (\n -> length n == 60) $! map digitFacChain [10000..100000]
main = print $ iterChainLengthTo 100000
