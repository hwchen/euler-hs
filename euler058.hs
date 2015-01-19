-- spiral primes

-- find ratio of prime diagonals to all diagonals in a spiral square
-- naive should be slow, because it redraws the spiral each time.

import Data.List
import Utils (isPrime)


diagOne :: [Int]
diagOne = [n*n | n <- [3,5..]]

diagTwo :: [Int]
diagTwo = [n*n - (n-1)| n <- [3,5..]]

diagThree :: [Int]
diagThree = [n*n - 2*(n-1) | n <- [3,5..]]

diagFour :: [Int]
diagFour = [n*n - 3*(n-1) | n <- [3,5..]]


--sqLength must be odd
diagsSquareLengthOf :: Int -> [Int]
diagsSquareLengthOf sqLength = concat [take limit diagOne, 
                                       drop 1 $ take limit diagTwo, 
                                       drop 1 $ take limit diagThree, 
                                       drop 1 $ take limit diagFour
                                       ]
    where limit = (sqLength + 1) `div` 2

primeRatio :: [Int] -> Double 
primeRatio xs = fromIntegral (length $ filter isPrime xs) / fromIntegral (length xs)


primeRatioLessThan :: Double -> Int
primeRatioLessThan percent = snd $ head $ dropWhile (\(x,_) -> x > percent) lengthPrimeRatioTuple
    where lengthPrimeRatioTuple = zip (map (primeRatio . diagsSquareLengthOf) [3,5..]) [3,5..]

-- round 2, try by building sums at each length
-- oops, I don't want sums, I want a counter


-- sum of all diagonals at each length
-- square side 1 is added in at the beginning
sumDiagonalsSquare :: [Int]
sumDiagonalsSquare = zipWith4 (\a b c d -> a + b + c + d) diagOne diagTwo diagThree diagFour

-- sum of diagonals at each length if prime
sumDiagonalsSquarePrime :: [Int]
sumDiagonalsSquarePrime = zipWith4 (sumPrimes) diagOne diagTwo diagThree diagFour
    where sumPrimes a b c d = sum $ filter isPrime [a,b,c,d]

-- accumulate sums 
accumSum :: [Int] -> [Int]
accumSum = scanl (+) 1 

-- round 3
-- counting instead of sum this time
-- counts start at side 3, side 1 is added in at accumCount to avoid duplication
-- don't need to memoize, use scan

-- # at each side is always 4
countDSq :: [Int]
countDSq = [4,4..]

accumCountAll :: [Int]
accumCountAll = scanl (+) 1 countDSq

countDSqPrime :: [Int]
countDSqPrime = zipWith4 (countPrimes) diagOne diagTwo diagThree diagFour
    where countPrimes a b c d =  length $ filter isPrime [a,b,c,d]

accumCountPrime :: [Int]
accumCountPrime = scanl (+) 0 countDSqPrime

sideRatioList :: [(Int,Double)]
sideRatioList = zipWith3 (sideRatio) [1..] accumCountPrime accumCountAll
    where sideRatio n x y = (2*n-1, (fromIntegral x) / (fromIntegral y))

findBelow10Percent :: Int
findBelow10Percent = fst $ head $ dropWhile (\(_,b) -> b > 0.1) $ drop 1 sideRatioList
--8.5 seconds

main :: IO()
--main = print $ primeRatioLessThan 0.1
main = do
    print findBelow10Percent
--    print $ take 10 $ accumCountPrime
--    print $ take 10 $ accumCountAll
--    print $ take 10 $ sideRatioList
