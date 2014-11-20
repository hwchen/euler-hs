--euler 10

-- sum of all primes below 2 million
import Utils (isPrime)

-- 5 minutes. Wait, it's 5 minutes using runhaskell.
-- at -O2, it only takes 18 seconds!!
sumPrimesBelow :: Integral a => a -> a
sumPrimesBelow limit = sum $ filter isPrime [1..limit]

main = print $ sumPrimesBelow 2000000
