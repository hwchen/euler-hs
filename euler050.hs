-- prime that can be written as the sum of consecutive primes.
-- find longest one under 1 million

-- I can't generate all chains. So I have to check each prime

-- I am able to start at chains longer than 21... and I can end
-- when chain sums to over target prime

-- so, I need to check for each target prime, because that constrains
-- the chains.

-- ah, for very long chains, the last number will be well below target n
-- in nexkfact, if chain is three numbers, it last number in chain will
-- be around 1/3. I'm looking for n bigger than 50. So I'll try bounding
-- primeChains by n `div` 50 first.

-- bound kind of helps, but the generation of chains is still too slow!

import Data.List
import Utils (isPrime)

primeList :: [Int]
primeList = filter isPrime [1..]

-- a chain is from starting n0 until sum >= target
-- this function generates all prime chains that add

--hasPrimeSumChain :: Int -> Bool
--hasPrimeSumChain n = not $ null $ primeChain n

-- duplicates takeWhile and filter. how to merge? Also what's the 
-- bound if there is no primeChain? This is the important point.
primeChainToFrom :: Int -> Int -> [Int]
primeChainToFrom b a = concat $ filter (\ns -> sum ns == b) chains 
    where chains = takeWhile (\xs -> sum xs <= b) $ -- slow, does sum for each
                   inits $ dropWhile (/= a) primeList

-- returns empty chain if no chain == target
primeChainToFrom' :: Int -> Int -> [Int]
primeChainToFrom' b a = go [a] a
    where go xs sum' | sum' == b = xs
                     | sum' < b = go (xs ++ [next]) (sum' + next)
                     | otherwise = []
                     where next = head $ dropWhile (< last xs) primeList

primeChains :: Int -> [[Int]]
primeChains n = map (primeChainToFrom n) $ takeWhile (<= n `div` 50) primeList

primeChainsMax :: Int -> Int
primeChainsMax n = case chainLengths of
    xs | not $ null xs -> maximum xs
       | otherwise -> 0
    where chainLengths = map length $ primeChains n

main :: IO()
main = print $ maximum $ zip (map primeChainsMax $ takeWhile (<1000000) primeList) (takeWhile (<1000000) primeList)
