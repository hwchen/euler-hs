-- euler 65 convergents of e

--denominator: gives add previous, then add previous to current*2, 4, 6.. , then add 
-- 1,1,3,4,7,32,39,71,465,536

-- basically, add previous to current, then add previous to current, then add previous to current * [2,4,6..]

import Utils (intToList)

-- wasn't needed for answer...
denominator :: [Integer]
denominator = 1 : period 0 1 2 

period :: Integer -> Integer -> Integer -> [Integer]
period prev init const = a : b : c : period b c (const+2)
    where a = prev + init 
          b = a * const + init 
          c = a + b 

-- numerator 2,3,8,11,19,87,106,193,1264,1457

numerator :: [Integer]
numerator = 2 : period 1 2 2

main :: IO()
main = print $ sum $ flip intToList 10 $ head $ drop 99 $ numerator
