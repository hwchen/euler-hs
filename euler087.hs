-- euler 87
-- prime power triples

-- how many numbers below 50 million can be expressed as sum of prime square,
-- prime cube, prime fourth

-- for ^2, limit of base is 7072
-- for ^3, limit of base is about 370
-- for ^4, limit of base is about 85

-- oops, didn't check for duplicates at first
-- oops, didn't map isPrime!

import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Utils (isPrime)

primeListTo :: Int -> [Int]
primeListTo limit = filter isPrime [2..limit]

primeList = filter isPrime [2..7072]

primeSq = [x*x | x <- primeList]
primeCu = [x*x*x | x <- takeWhile (<370) primeList]
primeFo = [x*x*x*x | x <- takeWhile (<85) primeList]

-- a is prime^2, b ^3, c ^4
genCombBase :: Set Int
genCombBase = S.fromList [a+b+c | a <- primeSq
                                , b <- primeCu
                                , c <- primeFo
                                , a+b+c < 50000000
                                ]

main :: IO()
main = do
    print $ S.size genCombBase 

    --14 seconds before filtering with isPrime. and adding nub?
    -- set is faster than nub
