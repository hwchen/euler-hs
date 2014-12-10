-- Euler 36
-- palindromes in both base 10 and 2.
-- sum all under 1,000,000o


isPalindrome :: Eq a => [a] -> Bool 
isPalindrome s = s == reverse s

intToList :: Integral a => a -> a -> [a] 
intToList x base = reverse $ go x 
    where 
        go 0 = []
        go y = let (a,b) = quotRem y base in [b] ++ go a



palindromeTenTwoSum :: Integer
palindromeTenTwoSum = sum $ filter isPalindromeTenTwo [1..1000000]
    where isPalindromeTenTwo x = 
              (isPalindrome $ intToList x 10) && (isPalindrome $ intToList x 2)

