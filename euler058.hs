-- spiral primes

-- find ratio of prime diagonals to all diagonals in a spiral square
-- naive should be slow, because it redraws the spiral each time.

import Utils (isPrime)


diagOne :: [Int]
diagOne = [n*n | n <- [1,3..]]

diagTwo :: [Int]
diagTwo = 1: [n*n - (n-1)| n <- [3,5..]]

diagThree :: [Int]
diagThree = 1: [n*n - 2*(n-1) | n <- [3,5..]]

diagFour :: [Int]
diagFour = 1: [n*n - 3*(n-1) | n <- [3,5..]]


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

main :: IO()
main = print $ primeRatioLessThan 0.1
