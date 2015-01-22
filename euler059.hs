-- xor decryption

-- key is 3 lower case characters
-- encrypted text is in file euler059Cipher.txt


import Control.Monad
import Data.Bits
import Data.Char
import Data.List.Split
import Data.List
import System.IO

type Key = (Int, Int, Int)

-- refactor into one-liner with bind
readEncrypted :: FilePath -> IO [Int] 
readEncrypted fileName = do
    h <- readFile fileName
    let content = map (read) (splitOn "," h)
    return content

genKeys :: [Key]
genKeys = [(a,b,c) | a <- [97..122], b <- [97..122], c <- [97..122]]

applyKey :: Key -> [Int] -> [Int] 
applyKey (a,b,c) (x:y:z:zs) = x `xor` a : y `xor` b : z `xor` c : applyKey (a,b,c) zs
applyKey (a,b,c) (x:y:ys) = x `xor` a : y `xor` b : applyKey (a,b,c) ys
applyKey (a,b,c) (x:xs) = x `xor` a : applyKey (a,b,c) xs 
applyKey (a,b,c) [] = []

applyAllKeys :: [Key] -> [Int] -> [[Int]]
applyAllKeys keys encrypted = map (flip applyKey encrypted) keys

toAscii :: [[Int]] -> [String]
toAscii unencrypteds = map (map chr) unencrypteds

findUnencrypted :: [String] -> [String]
findUnencrypted = filter (\str -> isInfixOf "the" str && 
                                  isInfixOf "and" str &&
                                  isInfixOf "was" str)

main = do
    encrypted <- readEncrypted "euler059Cipher.txt"
    let result = findUnencrypted $ toAscii $ applyAllKeys genKeys encrypted
    print $ sum $ map ord $ head result
    -- this is enough to find the right one. then add "was" to get just one
    -- message back. and convert to ascii and sum.
    -- took me about one hour to complete?
