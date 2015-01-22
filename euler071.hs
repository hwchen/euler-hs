-- reduced fractions

-- find fraction just left of 3/7 when d <= 1000000

-- rightmost fraction must be reduction of (2d-2)/2d

-- inefficiences: toList and fromList, sort. Don't reduce until the end?

-- can I generate only up to 3/7? 

import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Utils (divisorsList)

type Frac = (Int,Int)

-- only take n less than half d? Doesn't help memory
genFractions :: Int -> [Frac]
genFractions x = [(n,d) | d <- [1..x], n <- takeWhile (<d) [1..x]]

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
fractionList :: Int -> Set (Frac)
fractionList x = S.fromList $ filter (\(_,b) -> b <= x) $ map reduceFraction $ genFractions x

sortedDoubleFracList :: Int -> [(Double, Frac)]
sortedDoubleFracList x = sort $ map (\(n,d) -> ((fromIntegral n) / (fromIntegral d), (n,d))) $ S.toList $ fractionList x

rightOfThreeSeventh :: Int -> Frac
rightOfThreeSeventh x = snd $ last $ takeWhile (\(a,_) -> a < 0.4285714285714285) $ sortedDoubleFracList x

main :: IO()
main = print $ rightOfThreeSeventh 1000000

