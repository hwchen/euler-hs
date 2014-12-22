-- euler 43
-- pandigital substring div

-- d2d3d4 is div 2
-- d3d4d5 is div 3
-- d4d5d6 is div 5
-- d5d6d7 is div 7
-- d6d7d8 is div 11
-- d7d8d9 is div 13
-- d8d9d10 is div 17

-- filter first by d8d9d0 div 17, that will yield the smallest field to search
-- That doesn't work. So, instead use permutations. I don't have to sort, so it will
-- be efficient enough!

-- permutations make the most differene. Took 50 seconds though with -O2

import Data.List
import Utils (intToList, listToInt)

-- pandigital from 0 through n
isPandigital09 :: Int -> Bool
isPandigital09 n = (length $ nub $ intToList n 10) == 10 

isSubStringsDiv :: Int -> Bool
isSubStringsDiv n
    | d24 `rem` 2 == 0 && 
      d35 `rem` 3 == 0 &&
      d46 `rem` 5 == 0 &&
      d57 `rem` 7 == 0 &&
      d68 `rem` 11 == 0 &&
      d79 `rem` 13 == 0 &&
      d80 `rem` 17 == 0  
          = True
    | otherwise = False
    where nList = intToList n 10
          d2List = tail $ nList
          d3List = tail $ d2List
          d4List = tail $ d3List
          d5List = tail $ d4List
          d6List = tail $ d5List
          d7List = tail $ d6List
          d8List = tail $ d7List
          d24 = listToInt $ take 3 $ d2List
          d35 = listToInt $ take 3 $ d3List
          d46 = listToInt $ take 3 $ d4List
          d57 = listToInt $ take 3 $ d5List
          d68 = listToInt $ take 3 $ d6List
          d79 = listToInt $ take 3 $ d7List
          d80 = listToInt $ take 3 $ d8List

perm09 :: [[Int]]
perm09 = permutations [0,1,2,3,4,5,6,7,8,9]

isLast3Div17 :: Int -> Bool
isLast3Div17 n = case intToList n 10 of
    (_:_:_:_:_:_:_:x:y:z) -> (listToInt ([x,y] ++ z)) `rem` 17 == 0
    _                     -> False

panSubStrings09 :: [Int]
panSubStrings09 = filter isSubStringsDiv $
                  filter isLast3Div17 $
                  map listToInt $
                  filter (\n -> length n == 10) perm09

main :: IO()
main = print $ sum panSubStrings09
