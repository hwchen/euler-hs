-- counting summations
-- euler 076

-- how many different ways can one hundred be written
-- as the sum of at least two positive integers

-- 1 :: 0
-- 2 :: 1
-- 3 :: 2
-- 4 :: 4
-- 5 :: 6
-- 6 :: 10
-- 7 :: 14
-- 8 :: 21
-- 9 :: 29

testGen8 = [(a,b,c,d,e,f,g,h) | 
                                h <- [0..8]
                              , g <- takeWhile (<=h) [0..8]
                              , f <- takeWhile (<=g) [0..8]
                              , e <- takeWhile (<=f) [0..8]
                              , d <- takeWhile (<=e) [0..8]
                              , c <- takeWhile (<=d) [0..8]
                              , b <- takeWhile (<=c) [0..8]
                              , a <- takeWhile (<=b) [0..8]
                              , a+b+c+d+e+f+g+h == 8
                              ]

testGen7 = [(a,b,c,d,e,f,g) | 
                              g <- [0..8]
                              , f <- takeWhile (<=g) [0..8]
                              , e <- takeWhile (<=f) [0..8]
                              , d <- takeWhile (<=e) [0..8]
                              , c <- takeWhile (<=d) [0..8]
                              , b <- takeWhile (<=c) [0..8]
                              , a <- takeWhile (<=b) [0..8]
                              , a+b+c+d+e+f+g == 7
                              ]

testGen9 = [(a,b,c,d,e,f,g,h,i) | 
                                i <- [0..9]
                              , h <- takeWhile (<= i)[0..9]
                              , g <- takeWhile (<=h) [0..9]
                              , f <- takeWhile (<=g) [0..9]
                              , e <- takeWhile (<=f) [0..9]
                              , d <- takeWhile (<=e) [0..9]
                              , c <- takeWhile (<=d) [0..9]
                              , b <- takeWhile (<=c) [0..9]
                              , a <- takeWhile (<=b) [0..9]
                              , a+b+c+d+e+f+g+h+i == 9
                              ]
