-- euler 56, powerful digit sum

-- for a^b, find the number with the highest digit sum. a,b < 100
-- numbers are too large, need Integer


import Utils (intToList)

digitSum :: Integer -> Integer
digitSum n = sum $ intToList n 10

powers :: [Integer]
powers = [a^b | a <- [1..99], b <- [1..99]]

powerDigitSum :: [Integer]
powerDigitSum = map digitSum powers

main :: IO()
main = print $ maximum powerDigitSum
