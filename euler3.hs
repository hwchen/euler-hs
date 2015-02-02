-- euler 3
-- largest prime factor of 600851475143

import Utils (isPrime, divisorsList)

lPF :: Int -> Int
lPF = maximum . filter isPrime . divisorsList

main :: IO()
main = print $ lPF 600851475143
