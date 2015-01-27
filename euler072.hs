-- counting fractions
-- euler072

import Data.List
import qualified Data.Set as S
import Utils (divisorsList)

genFracs :: Int -> [(Int,Int)]
genFracs n = [(a,b) | b <- [1..n], a <- takeWhile (<b) [1..n]]

genFracs' :: Int -> [(Int,Int)]
genFracs' n = [(a,b) | b <- [1..n], a <- takeWhile (<b) [1..n `div` 2]]
-- generate 1/2?

reduceFrac :: (Int,Int) -> (Int,Int)
reduceFrac (_,0) = (0,0) -- not true, but convenient here
reduceFrac (0,_) = (0,0)
reduceFrac (n,d) = (n',d')
    where gcf = S.findMax $ 
                S.intersection (S.fromList $ divisorsList n) 
                               (S.fromList $ divisorsList d)
          n' = n `div` gcf
          d' = d `div` gcf

lenCountFracs :: Int -> Int
lenCountFracs = length . nub . map reduceFrac . genFracs

lenCountFracs' :: Int -> Int
lenCountFracs' = length . genFracs

main :: IO()
main = print $ zip [1..] $ zipWith (-)(map lenCountFracs [1..20]) (map lenCountFracs' [1..20])
--main = print $ length $ nub $ map reduceFrac $ genFracs 8
--main = print $ zip [1,2..20] $ map (length . nub . map reduceFrac . genFracs) [1,2..20] 
--main = print $ S.size $ S.fromList $ frac 10000
-- so, just generating all possible fractions is inefficient
-- ok, so I know that it starts with n choose k, then reduce. How do I know how many to reduce?
