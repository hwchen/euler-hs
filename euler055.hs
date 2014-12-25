-- Lychrel numbers

-- lychrell number means it doesn't become a palindrome through reverse and add process.
-- how mnay lychrell numbers below 10,000

import Utils (intToList, listToInt)

isPal:: Integer -> Bool
isPal n = n' == reverse n'
    where n' = intToList n 10

reverseAdd :: Integer -> Integer
reverseAdd n = n + n'
    where n' = listToInt $ reverse $ intToList n 10

--return True if isPalindrome returns False for all first 50 iterations.
-- whoops, forgot that some palindromic numbers are seed.
isLychrell :: Integer -> Bool
isLychrell n = not $ isAllFalse $ map isPal $ drop 1 $ take 50 $ iterate reverseAdd n
    where isAllFalse = foldr (\a acc -> a || acc) False
-- generate 50 iterations of reverse/add for all numbers to 10000

main :: IO()
main = print $ length $ filter isLychrell [1..9999]
