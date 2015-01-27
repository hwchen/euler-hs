-- euler 99
-- largest ase pair

-- file: euler099BaseExp.txt

import Data.List
import Data.List.Split

main :: IO()
main = do
    content <- readFile "euler099BaseExp.txt"
    print $ snd $ maximum $ zip (map (\(a,b) -> a^b) $ baseExpList content) [1..]

baseExpList :: String -> [(Integer,Integer)]
baseExpList = map (tuplify2 . map (\x -> read x :: Integer) . splitOn ",") . lines 
    where tuplify2 (x:y:xs) = (x,y) --didn't check all cases because input is known

-- works, buth 3 minutes! Come back to this again

