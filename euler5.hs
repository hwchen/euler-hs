-- Project Euler problem 5

--smallest positive number that is evenly divisible by all numbers
--from 1 - 20

--2520 is smallest num evenly divided by all numbers from 1-10.

-- Before, used a list from [lower..] consecutive integers,
-- lower bounded by some fraction of n! but it was way too slow

--find primes, multiply them. must be a multiple of that.
-- THis provided the biggest boost! Basically instant now.
-- seems like it's constant time? That means that the candidate
-- list is very very short.

isPrime :: Integral a => a -> Bool
isPrime x = foldr (&&) True $ map (\n -> x `rem` n /= 0) [2.. x-1]

productPrimes :: Integral a => [a] -> a
productPrimes = product . filter isPrime

candidateList :: Integral a => a -> [a]
candidateList n = map (* productPrimes [1..n]) [1..]

multipleFilter :: Integral a => a -> [a] -> [a]
multipleFilter 1 xs = xs
multipleFilter n xs = multipleFilter (n-1) $ filter (\x -> x `rem` n == 0) xs

smallestMultiple :: Integral a => a -> a
smallestMultiple n = head $ multipleFilter n $ candidateList n 

main = putStrLn $ show $ smallestMultiple 50
