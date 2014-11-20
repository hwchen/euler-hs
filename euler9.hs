-- euler 9
import Utils (cartProd, cartProd3)

-- naive, takes many minutes
pythagTriples' :: Integral a => a -> [(a,a,a)]
pythagTriples' limit = filter (\(x,y,z) -> x*x+y*y == z*z) $ pythagCandidates' limit

pythagCandidates' limit = [(x,y,z) | x <- [1..limit], y <- [2..limit], z <- [3..limit], x < y, y < z]

pythagTripleSumEqualTo' :: Integral a => a -> (a,a,a)
pythagTripleSumEqualTo' sum = head $ filter (\(x,y,z) -> x+y+z == sum) $ pythagTriples' (sum * sum)

-- method 2
-- generate candidates equal to sum first.
-- 4 minutes to solution, 14 minutes to generate full list (it's lazy)

pythagCandidatesSumTo' :: Integral a => a -> [(a,a,a)]
pythagCandidatesSumTo' sum = 
    [(x,y,z) | x <- [3..sum], y <- [4..sum], z <- [5..sum], x< y, y<z, x+y+z == sum]

-- method 3
-- constrain bottom limit: if b^2 and c^2 are largest possible, what is a?
-- square root of (c^2 - b^2). well, that only saves me a little.
-- with largest spread, z is half sum, y is less 1, x is 1. closest is all three are 1/3
-- It's not possible for c to be far from a and b.o
-- i need to narrow the range of choices, or figure out how to skip over sections.
-- difference between a and b is 1 at smallest, and sum/2 at largest
-- c is between s and sum/3
-- It's still 4 minutes, so need to come up with something better.

pythagCandidatesSumTo'' :: Integral a => a -> [(a,a,a)]
pythagCandidatesSumTo'' sum =
    [(x,y,z) | x <- [3..sum], y <- [4..sum `div` 2], z <- [sum `div` 3..sum], x< y, y<z, x+y+z == sum]

-- method three.
-- from looking at threads. THey just brute forced but stopped at 500.
-- this takes only 22 seconds. so upper bound was the most important.
pythagCandidatesSumTo :: Integral a => a -> [(a,a,a)]
pythagCandidatesSumTo sum =
    [(x,y,z) | x <- [3..sum `div` 2], y <- [4..sum `div` 2], z <- [sum `div` 3..sum `div` 2],
    x< y, y<z, x+y+z == sum]

isPythagTriple :: Integral a => (a,a,a) -> Bool
isPythagTriple (x,y,z) = x*x+y*y == z*z

pythagTripleSumTo :: Integral a => a -> (a,a,a)
pythagTripleSumTo sum = head $ filter isPythagTriple $ pythagCandidatesSumTo sum

multiplyTriple :: Integral a => (a,a,a) -> a
multiplyTriple (x,y,z) = x*y*z

main = print $ multiplyTriple $ pythagTripleSumTo 1000
--main = print $ last $ pythagCandidatesSumTo 1000
