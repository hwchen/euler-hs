-- euler 61 cyclical figurate numbers.
-- whoops, need to read more closely.
-- chain must be cyclic
-- and each polygonal type does not need to be represented in order.

-- generate chains, or go for all combinations?

import Utils (intToList)

polyList :: (Int -> Int) -> [Int]
polyList f = takeWhile (<10000) [f x | x <- [1..], f x > 999] 

triangleFn n = (n*(n+1)) `div` 2
squareFn n = n*n
pentaFn n = n*(3*n-1) `div` 2
hexaFn n = n*(2*n-1)
heptaFn n = n*(5*n-3) `div` 2
octaFn n = n*(3*n-2)

tri = polyList triangleFn
sq = polyList squareFn
pent = polyList pentaFn
hexa = polyList hexaFn
hept = polyList heptaFn
oct = polyList octaFn

-- find xlist whose numbers start with end of x-1 list 
-- then chain

--refactor using Maybe

chain :: Int -> Int -> [Int]
chain x y
    | xEnd == yFront = [x,y]
    | otherwise = []
    where xEnd   = drop 2 $ intToList x 10 -- specific to problem
          yFront = take 2 $ intToList y 10

-- takes first list and matches to each possible choice in second list
buildChains :: [Int] -> [Int] -> [[Int]]
buildChains xs ys = filter (/= init xs) $ map (\y -> init xs ++ (last xs) `chain` y) ys

-- chains to chains
-- tries all elements in first list against end of all chains in second list
cToC :: [Int] -> [[Int]] -> [[Int]]
cToC ys xss = concatMap (flip buildChains ys) xss

initializeList :: [Int] -> [[Int]]
initializeList xs = map (\x -> [x]) xs

main = do
    let initial = initializeList $ polyList triangleFn
        allChain = cToC oct $ cToC hept $ cToC hexa $ cToC pent $ cToC sq initial
    print allChain 
