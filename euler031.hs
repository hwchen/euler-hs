-- euler 31
-- coin sums

-- 1p, 2p, 5p, 10, 20p, 50p, 1l, 2l
-- how many ways to make 2l?

-- 1p from 0 to 200
-- 2p from 0 to 100
-- 5p from 0 to 40
-- 10p from 0 to 20
-- 20p from 0 to 10
-- 50p from 0 to 4
-- 1l from 0 to 2
-- 2l from 0 to 1

-- generate all possible combinations of number of coins except 2l
genCoinComb :: [(Int, Int, Int, Int, Int, Int, Int)]
genCoinComb = [(a,b,c,d,e,f,g) | a <- [0..200],
                                 b <- [0..100],
                                 c <- [0..40],
                                 d <- [0..20],
                                 e <- [0..10],
                                 f <- [0..4],
                                 g <- [0..2],
                                 a+(2*b)+(5*c)+(10*d)+(20*e)+(50*f)+(100*g) == 200
                                 ]
genCoinComb' :: [(Int, Int, Int, Int, Int, Int)]
genCoinComb' = [(b,c,d,e,f,g) | 
                                 b <- [0..100],
                                 c <- [0..40],
                                 d <- [0..20],
                                 e <- [0..10],
                                 f <- [0..4],
                                 g <- [0..2],
                                 (2*b)+(5*c)+(10*d)+(20*e)+(50*f)+(100*g) <= 200
                                 ]

main :: IO()
main = print $ length genCoinComb'
-- slow, almost 3 minutes. Could have sped up by calculating all possibilities
-- that were under 200, which assumes that all holes can be filled by 1p
-- now, only takes .696 seconds!
