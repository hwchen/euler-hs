-- euler 021
-- sum of amicable number under 10000.

import Utils (divisorsList)

properDivisors :: Integral a => a -> [a]
properDivisors 0 = []
properDivisors n = init $ divisorsList n

-- sum all amicable number pairs uner 10000, divide by 2 to eliminate pairs in 
--list twice

generateAmicablePair :: Integral a => a -> (a,a)
generateAmicablePair n
    | n == (sum $ properDivisors candidate)  && n /= candidate = (n,candidate)
    | otherwise = (0,0)
    where candidate = sum $ properDivisors n

amicablePairList :: Integral a => [(a,a)]
amicablePairList = filter (/= (0,0)) $ map generateAmicablePair [1..]

amicablePairListUnder :: Integral a => a -> [(a,a)]
amicablePairListUnder n = takeWhile (\(x,y) -> x < n && y < n) amicablePairList

sumTupleList :: Num a => [(a,a)] -> a
sumTupleList = foldr (\(x,y) acc -> x + y + acc) 0

main = print $ (sumTupleList $ amicablePairListUnder 10000) `div` 2
