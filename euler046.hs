-- euler 46 Goldbach Other conjecture

-- smallest composite number that can be written as
-- prime plus 2 * n^2

import Data.List
import qualified Data.Set as S
import Utils (isPrime)

primeList :: [Int]
primeList = filter isPrime [1..]

goldbachList :: [Int]
goldbachList = filter (not . isPrime) $ 
               filter odd [p + 2 * n ^2 | p <- take 1000 primeList, n <- [1..100]]

isGoldbach :: Int -> Bool
isGoldbach n = n `S.member` (S.fromList goldbachList)

oddCompositeList :: [Int]
oddCompositeList = filter (not . isPrime) $ filter odd [3..]

smallestNonGoldbach :: Int
smallestNonGoldbach = head $ filter (not . isGoldbach) oddCompositeList

main :: IO()
main = print smallestNonGoldbach
