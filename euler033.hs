-- digit cancelling fractions
-- euler 33

-- fractions are two digits, less than 1.
-- look for fractions where cancelling diagonals leads to correct reduction.
-- got to use sets again! This time for GCD and reducing fractions.

import qualified Data.Set as S
import Utils (intToList, divisorsList)

genFractions :: [(Int,Int)]
genFractions = [(a,b) | b <- [10..99], a <- takeWhile (<b) [10..99]]

-- returns original if doesn't cancel
digitCancel :: (Int,Int) -> (Int,Int)
digitCancel (a,b)
    | a1 == b2 && a2 == b1 = (1,1)
    | a1 == b2 = (a2,b1)
    | a2 == b1 = (a1,b2)
    | otherwise = (a,b)
    where a' = intToList a 10
          b' = intToList b 10
          a1 = head a'
          a2 = last a'
          b1 = head b'
          b2 = last b'

tupleFraction :: (Fractional a, Num a) => (a,a) -> a
tupleFraction (a,b) = a / b

isTrueCancel :: (Int,Int) -> Bool
isTrueCancel (a,b) = (tupleFraction $ tupleToReal $ digitCancel (a,b)) 
                     == (tupleFraction $ tupleToReal (a,b))
                     && (digitCancel (a,b) /= (a,b))
    where tupleToReal (x,y) = (fromIntegral x, fromIntegral y)

trueCancelList :: [(Int,Int)]
trueCancelList = filter isTrueCancel genFractions

fractionProduct :: [(Int,Int)] -> (Int,Int)
fractionProduct xs = case unzip trueCancelList of
    (xs,ys) -> (product xs, product ys)

reduceFraction :: (Int,Int) -> (Int,Int)
reduceFraction (a,b) = (a `div` gcd, b `div` gcd)
    where divisorsA = S.fromList $ divisorsList a
          divisorsB = S.fromList $ divisorsList b
          gcd       = S.findMax $ S.intersection divisorsA divisorsB

main :: IO()
main = print $ reduceFraction $ fractionProduct trueCancelList
