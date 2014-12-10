-- euler 35
-- circular primes under one million
-- circular prime: all permutations of prime are also prime
-- only 14 seconds compiled -O2. Forever in ghci.

import Utils (isPrime, intToList)
import Data.List (permutations)

primeList :: Integral a => a -> [a]
primeList n = filter isPrime [1..n]

listToInt :: (Show a, Integral a) => [a] -> Int
listToInt xs = read (concatMap (\x -> show x) xs) :: Int

rotations :: [a] -> [[a]]
rotations xs = take (length xs) $ rotations' xs
    where rotations' [] = []
          rotations' (y:ys) = (y:ys) : rotations' (ys ++ [y])

circularList :: (Show a, Integral a) => a -> [Int]
circularList n = map listToInt $ rotations $ intToList n 10

isCircularPrime :: (Show a, Integral a) => a -> Bool
isCircularPrime n = foldr (\a acc -> a && acc) True $ map isPrime $ circularList n

main = print $ length $ filter isCircularPrime $ primeList 1000000
