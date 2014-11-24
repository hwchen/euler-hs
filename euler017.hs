-- euler 17
-- count number of letters used to count 1 to 1000
-- includes and, doesn't include spaces and hyphens

-- 342 is 23 letters, 115 is 20 letters

-- took a while to figure out the hundreds cases. Some cool pattern matching.

import Utils (intToDigitList)

-- convert to list of integers
expandInt :: Integer -> [Integer]
expandInt = foldr (\x acc-> x * 10^(length acc): acc) [] . intToDigitList

lastTwoDigitsSpecialAddOne :: Integer -> Int
lastTwoDigitsSpecialAddOne n = case reverse $ intToDigitList n of
   (9:1:_) -> 1
   (7:1:_) -> 1
   (6:1:_) -> 1
   (4:1:_) -> 1
   _       -> 0

greaterThanHundredAddThree :: Integer -> Int 
greaterThanHundredAddThree n = case reverse $ intToDigitList n of 
    (0:0:x:_) -> 0
    _  
        | n >= 100 -> 3
        | otherwise -> 0

-- after figuring out my implementation, could have used just lengths
-- but it was easier to debug
basicIntToString :: Integer -> String
basicIntToString 1000 = "onethousand"
basicIntToString 900 = "ninehundred"
basicIntToString 800 = "eighthundred"
basicIntToString 700 = "sevenhundred"
basicIntToString 600 = "sixhundred"
basicIntToString 500 = "fivehundred"
basicIntToString 400 = "fourhundred"
basicIntToString 300 = "threehundred"
basicIntToString 200 = "twohundred"
basicIntToString 100 = "onehundred"
basicIntToString 90 = "ninety"
basicIntToString 80 = "eighty"
basicIntToString 70 = "seventy"
basicIntToString 60 = "sixty"
basicIntToString 50 = "fifty"
basicIntToString 40 = "forty"
basicIntToString 30 = "thirty"
basicIntToString 20 = "twenty"
basicIntToString 10 = "ten"
basicIntToString 9 = "nine"
basicIntToString 8 = "eight"
basicIntToString 7 = "seven"
basicIntToString 6 = "six"
basicIntToString 5 = "five"
basicIntToString 4 = "four"
basicIntToString 3 = "three"
basicIntToString 2 = "two"
basicIntToString 1 = "one"
basicIntToString 0 = ""

numToLetters = concatMap basicIntToString . expandInt

sumLettersOfNumber :: Integer -> Int
sumLettersOfNumber n = 
    (length $ numToLetters n) + (lastTwoDigitsSpecialAddOne n) + (greaterThanHundredAddThree n)

main = print $ sum $ map sumLettersOfNumber [1..1000]
--subtract 27 for each hundred with no follow
