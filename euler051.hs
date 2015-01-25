-- prime digit replacements

-- smallest prime which, by replacing part of the number (not
-- necessarily adjacent) with the same digit, is part of an
-- eight prime value family.

-- n is length of digit

import Data.List
import Utils (intToList, listToInt, isPrime)

-- last spot can never be replaced. Also, first spot can never be 0.
-- combinatorial spot is usually the cause of the big slowdowns.
replacementSpots :: Int -> [(Int,Int)]
replacementSpots lengthN = [(a,b) | b <- [1..lengthN-1], a <- takeWhile (<b) [1..lengthN-1]]

-- replace digit at a in integer n, with digit x
-- maybe refactor to do two at once is faster?
replaceDigit :: Int -> Int -> Int -> Int
replaceDigit n a x = listToInt $ take (a-1) n' ++ [x] ++ drop a n'
    where n' = intToList n 10

-- n is number to generate family from. Tuple is spots to replace
-- so ugly! but it'll do for now.
generateFamily :: Int -> (Int,Int) -> [Int]
generateFamily n (a,b) = filter isPrime $ map (replaceDigits) [0,1..9]
    where replaceDigits x = replaceDigit (replaceDigit n b x) a x 
          

generateFamilies :: Int -> [[Int]]
generateFamilies n = map (generateFamily n) $ replacementSpots $ length (intToList n 10)

-- raise starting point, because otherwise get tons of duplicates when replacing
-- two spots
-- how to reduce the numbers checked? Whoops, of course first number should be prime
allFamiliesTo :: Int -> [[Int]]
allFamiliesTo limit = concatMap generateFamilies $ filter isPrime [1000000.. limit]

findFamilySeven :: [[Int]] -> [Int]
findFamilySeven = head . filter (\n -> length n ==8)

main :: IO()
main = print $ findFamilySeven $ allFamiliesTo 10000000

