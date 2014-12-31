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
primeChainToFrom' :: Int -> Int -> [Int]
primeChainToFrom' b a = concat $ filter (\ns -> sum ns == b) chains 
    where chains = takeWhile (\xs -> sum xs <= b) $ -- slow, does sum for each
                   inits $ dropWhile (/= a) primeList

-- returns empty chain if no chain == target
primeChainToFrom'' :: Int -> Int -> [Int]
primeChainToFrom'' b a = go [a] a
    where go xs sum' | sum' == b = xs
                     | sum' < b = go (xs ++ [next]) (sum' + next)
                     | otherwise = []
                     where next = head $ dropWhile (<= last xs) primeList

primeChains' :: Int -> [[Int]]
primeChains' n = map (primeChainToFrom' n) $ takeWhile (<= n `div` 2) primeList

primeChainsMax' :: Int -> Int
primeChainsMax' n = case chainLengths of
    xs | not $ null xs -> maximum xs
       | otherwise -> 0
    where chainLengths = map length $ primeChains' n

-- upperBound = one million. Don't run sum, just generate, it will
-- probably be faster.

-- well, this way looks faster.

-- I should still generate only up to target sum though.

-- hmmm.... It looks like I blindly generated an extremely high sum bound.
-- when ChainTo is 10000, the sum Bound is already over 5 million. That means
-- that when I increased by chainTo bound to 1 million, I was generating
-- a super high sum bound. So, I don't know exactly what the ballpark is
-- but I can keep the chainBound at 1000 and just set a separate sumBound

-- interesting, if I set the chainBound at 1000, I get wrong answer.
-- this means that the chain goes past 1000 (or starts past).

-- filter isPrime after sumBound improves from 26 to 20 seconds.
-- (instead of checking both together in one filter)

chainsFromTo :: Int -> Int -> [[Int]]
chainsFromTo n0 n = inits $ takeWhile (<= n) $ dropWhile (< n0) primeList

allChainsTo :: Int -> [[Int]]
allChainsTo n = concatMap (\n0 -> chainsFromTo n0 n) $ takeWhile (<= n) primeList 

tupleChainLengthSum :: Int -> [(Int,Int)]
tupleChainLengthSum upperBound = map (\xs -> (length xs, sum xs)) $ allChainsTo upperBound  

maxTupleChainLengthSum :: Int -> Int -> (Int,Int)
maxTupleChainLengthSum chainBound sumBound= maximum $ 
                                            filter (\(a,b) -> isPrime b) $
                                            filter (\(a,b) -> b < sumBound) $ 
                                            tupleChainLengthSum chainBound

main :: IO()
main = print $ maxTupleChainLengthSum 10000 1000000
