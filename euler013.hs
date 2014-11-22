-- euler 13

-- first ten digits of the sum of 100 50-digit numbers.

import System.IO

parseToList :: String -> [Integer]
parseToList = map (\n -> read n :: Integer) . lines

main = do
    h <- openFile "euler013NumFile.txt" ReadMode
    numString <- hGetContents h
    putStrLn $ take 10 $ show $ sum $ parseToList numString
    -- why do I need to hclose last?
    -- lazy IO. doesn't read until needed. I think. Check on this.
    hClose h

