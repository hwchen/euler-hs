-- euler 62
-- cube permutations
-- smallest cube for which exactly 5 permutations of its
-- digits are cube.

-- 345 cubed is permutation of 384 cubed and 405 cubed.
-- answer 127035954683


import Data.List
import Utils (intToList, listToInt)

cubeList :: [Int]
cubeList = [x^3 | x <- [1..]]

cubeListAsDigits :: [[Int]]
cubeListAsDigits = map (flip intToList 10) cubeList

cubeListAsSortedDigits :: [[Int]]
cubeListAsSortedDigits = map sort cubeListAsDigits

zipCubeAndSortedDigits :: [(Int, [Int])]
zipCubeAndSortedDigits = zip cubeList cubeListAsSortedDigits

findPermutations :: Int -> [(Int,[Int])]
findPermutations  n = filter (\(a,b) -> b == n') $ 
                      takeWhile (\(x,y) -> x <= limit) zipCubeAndSortedDigits
    where n' = sort $ intToList n 10
          limit = listToInt $ reverse $ sort $ intToList n 10

genPermutations :: [[(Int,[Int])]]
genPermutations = map findPermutations cubeList

findSmallestNPermutation :: Int -> [(Int,[Int])]
findSmallestNPermutation n = head $ filter (\xs-> length xs == n) genPermutations

smallestOfSNP :: Int -> Int
smallestOfSNP n = case findSmallestNPermutation n of
    []         -> 0
    ((a,b):xs) -> a
