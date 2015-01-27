-- reduced fractions

-- find fraction just left of 3/7 when d <= 1000000

-- rightmost fraction must be reduction of (2d-2)/2d

-- inefficiences: toList and fromList, sort. Don't reduce until the end?

-- can I generate only up to 3/7? 
-- memoize divisorsList?

import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Utils (divisorsList)

type Frac = (Int,Int)

-- only take n less than half d? Doesn't help memory
-- of course, just generate those less than 3/7
-- as numbers get higher and higher, d of fraction next to 3/7 approaches x
-- here, we'll just bootstrap from the last 100 possible d
-- boxing possible n on both sides was the final optimization
genFractions :: Int -> [Frac]
genFractions x = [(n,d) | d <- [x-100..x]
                 , n <- takeWhile (<d) [x `div` 3..x `div` 2]
                 , (fromIntegral n) / (fromIntegral d) < 0.428571428571428
                 , (fromIntegral n) / (fromIntegral d) > 0.375]

reduceFraction :: Frac -> Frac
reduceFraction (_,0) = (0,0) -- not true, but convenient here
reduceFraction (0,_) = (0,0)
reduceFraction (n,d) = (n',d')
    where gcf = S.findMax $ 
                S.intersection (S.fromList $ divisorsList n) 
                               (S.fromList $ divisorsList d)
          n' = n `div` gcf
          d' = d `div` gcf

-- list of reduced fractions with d <= x
-- i had put reducefractions here, too early. Only need to reduce at the end!
fractionList :: Int -> [Frac]
fractionList x = filter (\(_,b) -> b <= x) $ genFractions x

-- doing toList and fromList takes too much memory. Doesn't need to be a set,
-- since the sort is done by Double
doubleFracList :: Int -> [(Double, Frac)]
doubleFracList x = sort $ map (\(n,d) -> ((fromIntegral n) / (fromIntegral d), (n,d))) $ fractionList x

rightOfThreeSeventh' :: Int -> Frac
rightOfThreeSeventh' x = snd $ last $ sort $ filter (\(a,_) -> a < 0.4285714285714285) $ doubleFracList x

rightOfThreeSeventh :: Int -> Frac
rightOfThreeSeventh x = reduceFraction $ snd $ last $ sort $ doubleFracList x

main :: IO()
main = print $ rightOfThreeSeventh 1000000

