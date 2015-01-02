-- euler 60
-- prime pair sets.

-- find a set of 5 primes which, when any two are concatenated, which gives 
-- lowest sum

-- maybe I should build sets, from all sets of two, then all of three?

-- or just do 5 straightaway.

import qualified Data.Set as S
import Utils (intToList, isPrime, listToInt)

primeList = filter isPrime [1..100000000]
primeSet = S.fromList primeList 

isPrime' :: Int -> Bool
isPrime' n = n `S.member` primeSet

--

isPrimePair :: Int -> Int -> Bool
isPrimePair x y = isPrime pair && isPrime pair'
    where pair = listToInt (intToList x 10 ++ intToList y 10)
          pair' = listToInt (intToList y 10 ++ intToList x 10)

primePairSet2 :: Int -> [(Int,Int)]
primePairSet2 limit = [(a,b) | b <- takeWhile (<limit) primeList
                             , a <- takeWhile (<b) primeList
                             , isPrimePair a b
                             ]

primePairSet3 :: Int -> [(Int, Int, Int)]
primePairSet3 limit = [(a,b,c) | c <- takeWhile (<limit) primeList
                               , (a,b) <- takeWhile (\(x,y) -> y < c) $ primePairSet2 limit
                               , isPrimePair a c
                               , isPrimePair b c
                               ]

primePairSet4 :: Int -> [(Int, Int, Int, Int)]
primePairSet4 limit = [(a,b,c,d) | d <- takeWhile (<limit) primeList
                               , (a,b,c) <- takeWhile (\(x,y,z) -> z < d) $ primePairSet3 limit
                               , isPrimePair a d
                               , isPrimePair b d
                               , isPrimePair c d 
                               ]

primePairSet5 :: Int -> [(Int, Int, Int, Int, Int)]
primePairSet5 limit = [(a,b,c,d,e) | e <- takeWhile (<limit) primeList
                               , (a,b,c,d) <- takeWhile (\(x,y,z,z') -> z' < e) $ primePairSet4 limit
                               , isPrimePair a e
                               , isPrimePair b e
                               , isPrimePair c e
                               , isPrimePair d e
                               ]

--from 1000 to 2000, time went from 1 to 10 seconds. That's a huge order of magnitude increase.
-- 3000 is 30 seconds. 4000 is 1min

-- generating 5000 combinations for 2 is only .025s. with primePair2, it takes 12s. Big diff!
-- does that mean that reversing concatenation is more expensive than generating pairs?
-- from 5000 to 7000 doesn't increase the time much though? both about 4:40.
-- finished in 13 minutes, up to 10,000.



main :: IO()
main = 
    --print $ length [(a,b) | b <- takeWhile (<5000) primeList
    --                         , a <- takeWhile (<b) primeList
    --                         ]
    --print $ primePairSet5 10000
    print $ primePairSet3 1000
