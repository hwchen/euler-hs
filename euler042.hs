-- triangle words

--triangle is tn = .5n(n+1)
-- convert each word to its numerical value of each letter.
-- if that converts to a triangle number, its a triangle word

-- find max wordSum for max limit

import Data.Char
import Data.List.Split
import Utils (alphaToNum) 

wordList :: FilePath -> IO [String]
wordList path = do
    content <- readFile path
    let res = splitOn "," content
    return res

cleanWordList :: [String] -> [String]
cleanWordList s = map (map toLower) $ map (filter (/= '\"')) s

sumWord :: String -> Int
sumWord = sum . map alphaToNum

sumWordList :: [String] -> [Int]
sumWordList = map sumWord

triangleList :: Int -> [Int]
triangleList upperBound = takeWhile (<= upperBound) [(n*(n+1) `div` 2) | n <- [1..]]

-- kind of messy, not very efficient, but works.
isTriangle :: Int -> Int -> Bool
isTriangle upperBound n = n `elem` (triangleList upperBound)

main = do
    content <- wordList "euler042Words.txt"
    print $ length $ filter (isTriangle 192) $ sumWordList $ cleanWordList content
