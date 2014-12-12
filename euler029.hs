-- euler 29 distinct powers
-- requires Integer, for very large ints

import Data.List

distinctPowers :: Integer -> [Integer]
distinctPowers n = sort $ nub $ [a^b | a <- [2..n], b <- [2..n]]
