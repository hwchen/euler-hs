-- euler 40
-- champernowne's constant
-- make an irrational decimal fraction by concatenating
-- positive integers. 0.1234567891011121314151617181920...
-- 12th digit is 1

-- find digit 1 * digit 10 * digit 100 ... * digit 1,000,000

-- kind of ugly, but straightforward and works. (and simple to understand).

import Utils (intToList)

posIntFraction :: [Int]
posIntFraction = concatMap (\n -> intToList n 10) [1..]

d1 = head posIntFraction
d10 = head $ drop 9 posIntFraction
d100 = head $ drop 99 posIntFraction
d1000 = head $ drop 999 posIntFraction
d10000 = head $ drop 9999 posIntFraction
d100000 = head $ drop 99999 posIntFraction
d1000000 = head $ drop 999999 posIntFraction

result :: Int
result = d1 * d10 * d100 * d1000 * d10000 * d100000 * d1000000
