-- euler 57 square root convergents

-- 1 + 1/2
-- 1 + 1/(2 + 1/2)
-- 1 + 1/(2 + 1/(2 + 1/2)

-- don't need to worry about the outcome
-- I want to find the expansions where
-- number of digits in the numerator > digits in denominators

-- denominator goes from [2, 5, 12, 29, 70, 169, 408]
-- That's previous times 2 plus the previous

-- numerator goes from [3, 7, 17, 41, 99, 239, 577]
-- it's related to denominator, you can get it by adding
-- denominator n + (n-1).

-- only half an hour, once I figured out that I need to look at
-- the sequence of upper and lower separately.

import Utils (intToList)

denominator :: [Integer]
denominator = [2,5] ++  [x+2*y | (x,y) <- zip denominator (tail denominator)]

numerator :: [Integer]
numerator = map (\(a,b) -> a+b) $ ((3,0) : (zip denominator (tail denominator)))

fraction :: [(Integer, Integer)]
fraction = zip numerator denominator

fractionDigitLength :: [(Int, Int)]
fractionDigitLength = map (\(a,b) -> (length $ intToList a 10, length $ intToList b 10)) fraction

main :: IO()
main = print $ length $ filter (\(a,b) -> a > b) $ take 1000 fractionDigitLength
