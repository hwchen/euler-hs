module Utils 
    (isPrime,
    sieveSundaram,
    sieveErast,
    squaresList,
    cartProd,
    cartProd3,
    intToDigitList,
    divisorsList,
    alphaToNum,
    intToList,
    fac,
    listToInt
    ) where

import Data.Char
import Data.List

squaresList :: Integral a => [a]
squaresList = map (\x -> x*x) [1..]

-- fun, but is slow because checks every single number in list
isPrime' :: Integral a => a -> Bool
isPrime' 1 = False 
isPrime' n = foldr (&&) True $ map (\x -> n `rem` x /= 0) [2..n-1]

isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime n = go 2
    where go x 
            | x * x > n = True 
            | n `rem` x == 0 = False 
            | x `rem` 2 == 0 = go (x+1)
            | otherwise = go (x+2)

--sieve
sieveSundaram :: Integral a => a -> [a]
sieveSundaram n = map (\n -> 2*n+1) $ filter (`notElem` filterList) [1..limit]
    where numList = cartProd [1..limit] [1..limit]
          filterList = map (\(x,y) -> (x+y+(2*x*y))) numList
          limit = 2*n+2

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

cartProd3 :: [a] -> [b] -> [c] -> [(a,b,c)]
cartProd3 xs ys zs = [(x,y,z) | x <- xs, y <- ys, z <- zs]

--sieveErastosthenes
sieveErast :: Integral a => a -> [a]
sieveErast n = sieveErast' n [1..n]

sieveErast' :: Integral a => a -> [a] -> [a]
sieveErast' 1 xs = tail xs
sieveErast' n xs
    | isPrime n = sieveErast' (n-1) $ (filter (\x -> x == n || x `rem` n /= 0) xs)
    | otherwise = sieveErast' (n-1) xs


-- convert Integer to Digits

--through string
intToDigitList :: (Show a, Integral a) => a -> [Integer]
intToDigitList n = map (\x -> read [x] :: Integer) (show n)

-- divisorsList (refactor the nub out later)
divisorsList :: Integral a => a -> [a]
divisorsList 0 = []
divisorsList n = nub (divisorsListHalf ++ map (n `div`) (reverse divisorsListHalf))
    where divisorsListHalf = filter (\x -> n `rem` x == 0) [1..floor $ sqrt $ fromIntegral n]

--convert Alphabet to Int, case insensitive
alphaToNum :: Char -> Int
alphaToNum c
    | lowerC `elem` ['a'..'z'] = (fromEnum $ toLower c) - 96
    | otherwise = 0
    where lowerC = toLower c

-- convert Integer to List of Integers, with argument for base
intToList :: Integral a => a -> a -> [a]
intToList x base = reverse $ go x
    where go 0 = []
          go y = let (a,b) = quotRem y base in [b] ++ go a

-- factorial
fac :: Integral a => a -> a
fac 0 = 1
fac x = product [1..x]

-- convert list of Integers to Integer
-- uses strings, so not most efficient
listToInt :: (Show a, Integral a) => [a] -> Int
listToInt xs = read (concatMap (\x -> show x) xs) :: Int
