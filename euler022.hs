--euler22
--name score

import Data.List
import Data.List.Split (splitOn)
import Utils (alphaToNum)

nameScore :: String -> Int
nameScore = sum . map alphaToNum 

main = do
    contents <- readFile "euler022Names.txt"
    let names = sort $ splitOn "," contents
        namesIndexed = zip [1..] names
    print $ sum $ map (\(x,y) -> x * (nameScore y)) namesIndexed
