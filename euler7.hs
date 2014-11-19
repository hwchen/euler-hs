-- Euler 7

-- find 10001st prime

import Utils (isPrime)

-- takes 8 min with naive isPrime (not stopping at sqrt with naive isPrime (not stopping
--at sqrt) WIth optimization, takes only 6s.
-- optimization was not totally obvious. I'd done it before when checking division,
-- but forgot about it in the context of Haskell, where i'm used to checking full
-- lists. Also, didn't know how to do ++ in haskell, most fns are -- recursion.
primeN :: Integral a => Int -> a
primeN n = last $ take n $ filter isPrime [1..]

main = putStrLn $ show $ primeN 10001
