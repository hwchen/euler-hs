-- permuted multiples

-- first integer whose multliples map (*x) [2..6] are all permutations

import Data.List
import Utils (intToList)

arePerm :: Int -> Int -> Bool
arePerm a b = a' == b'
    where a' = sort $ intToList a 10
          b' = sort $ intToList b 10

genMultiples :: Int -> [Int]
genMultiples n = map (*n) [1..6]

--fold to compare
multiplePerms :: [Int] -> Bool
multiplePerms (x:xs) = isAllTrue $ map (arePerm x) xs
    where isAllTrue = foldr (\a acc -> a && acc) True 

minInt :: Int
minInt = head $ head $ filter multiplePerms $ map genMultiples [1..]

main :: IO()
main = print minInt
