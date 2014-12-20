-- euler 27 Quadratic Primes

-- n^2 + an + b for a < 1000, b < 1000

-- a bit messy because I had to get back the pair as an answer

import Data.List
import Utils (cartProd, isPrime)

type Pair = (Int, Int)

isPrime' :: Int -> Bool
isPrime' x = isPrime x && (x >= 0)

consecPrimes :: (Pair,[Int]) -> (Pair, Int)
consecPrimes (ab, xs) = (ab, length $ takeWhile isPrime' xs) 

genQuadPrimes :: Pair -> (Pair, [Int])
genQuadPrimes (a, b) = ((a,b), [n^2 + a*n + b | n <- [0..]])

filterEvenPairs :: [Pair] -> [Pair]
filterEvenPairs = filter (\(a,b) -> odd a && odd b)

quadPrimes :: [(Pair, [Int])]
quadPrimes = map genQuadPrimes $ filterEvenPairs $ cartProd [-1000..1000] [-1000..1000]

maxQuadPrimesAB :: (Pair, Int)
maxQuadPrimesAB = maximum' $ map consecPrimes quadPrimes
    where maximum' = foldl1 max'
          max' ab@( _ , x) ab'@( _ , x') | x > x' = ab
                                 | otherwise = ab'

main = print $ maxQuadPrimesAB
