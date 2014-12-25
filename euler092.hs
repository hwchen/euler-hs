-- square digit chains
-- euler 92

-- with filter and map, takes 25 seconds
-- just filter, it's the same. So ghci performs optimizations!


-- I got a bit tripped up on using && in the dropwhile. i originally
-- thought it was ||, but that would be for a takewhile type situation.

import Utils (intToList)

squareDigits :: Int -> Int
squareDigits n = sum $ map (^2) $ intToList n 10

numberChainEnd :: Int -> Int
numberChainEnd n
    | head chainEnd == 89 = 89
    | head chainEnd == 1 = 1
    where chainEnd = dropWhile (\x -> (x /= 89) && (x /= 1)) $ iterate squareDigits n

main :: IO()
main = print $ length $ filter (\n -> numberChainEnd n == 89) [1..10000000]
