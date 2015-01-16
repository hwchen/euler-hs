-- euler 61 cyclical figurate numbers.
-- whoops, need to read more closely.
-- chain must be cyclic
-- and each polygonal type does not need to be represented in order.

-- generate chains, or go for all combinations?

import Data.List
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

-- Find all cyclic chains first.
-- all chains have to pass through octa, so use that as seed!
-- at each step of chain reduce pool? or check afterwards?

-- works only for comparing first to to last to of 4-digit numbers
appendToChain :: [Int] -> Int -> [Int]
appendToChain ns n
    | drop 2 lastOfNs' == take 2 n' = ns ++ [n]
    | otherwise                     = []
    where lastOfNs' = intToList (last ns) 10
          n'        = intToList n 10

-- initial values from oct list, and mapped to lists to start chain
initValues :: [Int] -> [[Int]]
initValues = map (\n -> [n])

-- takes seed chain and list of possible appends, returns new chains
chainFromSeed :: [Int] -> [Int] -> [[Int]]
chainFromSeed ns = filter (/= []) . map (appendToChain ns)

-- generate all chains starting from octa values
notOct = concat [tri, sq, pent, hexa, hept]

-- test to see what generating all chains from oct does
twoChains :: [[Int]]
twoChains = concatMap (flip chainFromSeed notOct) (initValues oct)

-- generate all chains of length 6
allChains :: [[Int]]
allChains =  concat $ drop 5 $ take 6 $ iterate (concatMap (flip chainFromSeed notOct)) (initValues oct)

-- check for cyclic chains 
isCyclic :: [Int] -> Bool
isCyclic xs
    | drop 2 lastOfXs' == take 2 firstOfXs' = True
    | otherwise                             = False
    where lastOfXs' = intToList (last xs) 10
          firstOfXs' = intToList (head xs) 10

-- test for membership in each set of figurative
-- any of xs is an elem of ys
anyIsElemOf :: [Int] -> [Int] -> Bool
anyIsElemOf xs ys = foldr (anyTrue) False $ map (`elem` ys) xs
    where anyTrue a acc = a || acc

hasAllFigurative :: [Int] -> Bool
hasAllFigurative xs = anyIsElemOf xs tri &&
                      anyIsElemOf xs sq && 
                      anyIsElemOf xs pent &&
                      anyIsElemOf xs hexa && 
                      anyIsElemOf xs hept && 
                      anyIsElemOf xs oct 

-- after inspecting output, noticed that there can be duplicates
-- between hexa and tri lists
triAndHexaNotSame :: [Int] -> Bool
triAndHexaNotSame xs = filter (`elem` tri) xs /= filter (`elem` hexa) xs

testList :: [Int]
testList = [9633,3367,6724,2415,1540,4096] 

-- interesting, it actually runs faster when sum than when just printing 
-- out the lists
main = do
    print $ sum $ head $
            filter triAndHexaNotSame $ 
            filter hasAllFigurative $ 
            filter isCyclic allChains 
