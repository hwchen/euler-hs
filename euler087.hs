-- euler 87
-- prime power triples

-- how many numbers below 50 million can be expressed as sum of prime square,
-- prime cube, prime fourth

import Utils (isPrime)

limit = 50

primeList = [1..limit]

primeSq = [x*x | x <- primeList]
primeCu = [x*x*x | x <- primeList]
primeFo = [x*x*x*x | x <- primeList]

main :: IO()
main = do
    print primeSq
    print primeCu
    print primeFo
