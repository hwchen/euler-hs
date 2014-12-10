-- self powers
-- find last ten digits of the sum [1^1.. n^n]
-- didn't expect it to be so fast, but it was.

selfPowers :: Integral a => a -> a
selfPowers n = sum $ [x^x | x <- [1..n]]
