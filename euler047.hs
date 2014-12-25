-- distinct primes factors

--four consecutive integers that each have 4 distinct prime factors.

-- first, generate all divisors?

-- init and tail only work for divisorList with more than 1! (so 1 doesn't work)

-- lower bound: first 4 primes multiplied. 2*3*5*7 = 210

-- I didn't think it should take so long since I'm not doing permutations,
-- just generating a short list per n.

-- turns out I forgot 'head', so never evaluated...

import Utils (divisorsList, isPrime)

has4PrimeFactors :: Int -> Bool
has4PrimeFactors n = (length $ filter isPrime factors) >= 4
    where factors = init $ tail $ divisorsList n

gen4Consec :: Int -> [Int]
gen4Consec n = take 4 $ iterate (+1) n

is4Consec4Primes :: Int -> Bool
is4Consec4Primes n = isAllTrue $ map has4PrimeFactors $ gen4Consec n
    where isAllTrue = foldr (\a acc -> a && acc) True

main :: IO()
main = print $ head $ filter is4Consec4Primes [210..] 
