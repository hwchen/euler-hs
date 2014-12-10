-- Euler 30
-- numbers that can be written as the sum of the 5th power of their digits
-- how do I know what the upper bound really is? I just guessed...

import Utils (intToList)

sumDigitFifth :: Integral a => a -> a
sumDigitFifth x = sum $ map (^5) $ intToList x 10

sumDigitFifthList :: Integral a => [a]
sumDigitFifthList = tail $ filter (\n -> n == sumDigitFifth n) [1..1000000]

main = print $ sum sumDigitFifthList
