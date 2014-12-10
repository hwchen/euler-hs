-- euler 35
-- circular primes under one million
-- circular prime: all permutations of prime are also prime
-- only 14 seconds compiled -O2. Forever in ghci.

-- if I want to improve, any number that contains an even digit
-- or 0 or 5 cannot be a circular prime.
-- someday I'll come back and get it down to 2 seconds. Maybe
-- I have too many conversions. For now,
-- I love the expressiveness of the code.

import Utils (isPrime, intToList)

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
