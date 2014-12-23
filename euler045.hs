-- triangular, pentagonal, hexagonal

-- find next number over 40755 that is all three

-- wow, took 53 seconds even compiler -O2.

tNums :: [Int]
tNums = [n * (n+1) `div` 2 | n <- [1..]]

pNums :: [Int]
pNums = [n * (3*n-1) `div` 2 | n <- [1..]]

hNums :: [Int]
hNums = [n * (2*n-1)| n <- [1..]]

tphNums :: [Int]
tphNums = filter (\x -> x `elem` (tNums' x) && x `elem` (pNums' x)) $ hNums'
    where hNums'   = dropWhile (< 40755) hNums
          pNums' n = takeWhile (<= n) $ dropWhile (< 40755) pNums
          tNums' m = takeWhile (<= m) $ dropWhile (< 40755) tNums

main :: IO()
main = print $ head $ tail $ tphNums
