-- Pandigital Prime, euler 41
-- find largest pandigital prime.

-- generate pandigitals. then test for primality.

import Data.List
import Utils (isPrime, intToList, listToInt)

panDigitalOneTo :: Int -> [Int]
panDigitalOneTo n = map listToInt $ permutations [1.. n]

main :: IO()
main = print $ maximum $ filter isPrime $ concatMap panDigitalOneTo [1..9]
