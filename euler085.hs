-- euler 85
-- counting rectangles

-- (1,1) = 1
-- (1,2) = 3
-- (1,3) = 6
-- (1,4) = 10
-- (1,5) = 15
-- (1,6) = 21
-- each time add n to previous total


-- (2,1) = 3
-- (2,2) = 9
-- (2,3) = 18
-- (2,4) = 30
-- for (a,b), if increase b, add b^a to previous total?

-- (3,1) = 6
-- (3,2) = 18
-- (3,3) = 

-- let's try first with rectangles that are of (1,x)

import Data.List (scanl)

countRectOne :: [Int]
countRectOne = scanl (+) 1 [2,3..]

rectOne :: [(Int,Int)]
rectOne = zip [1..] countRectOne

countRectTwo :: [Int]
countRectTwo = scanl (\acc x -> acc + x*3) 3 [2,3..]

rectTwo:: [(Int,Int)]
rectTwo = zip [1..] countRectTwo

-- any reason to believe a certain ratio will give close to 2,000,000?
-- a square?

countRectThree:: [Int]
countRectThree = scanl (\acc x -> acc + x*6) 6 [2,3..]

rectThree:: [(Int,Int)]
rectThree = zip [1..] countRectThree
-- gets (816, 2000016)

-- generate all (a,b) combinations?

-- for rect(a,b), generate family of a
rectA :: Int -> [(Int,Int)]
rectA a = zip [1..] $ scanl (\acc x -> acc + x*a') a' [2,3..]
    where a' = sum [1..a]

rectACloseToTwoMil :: Int -> (Int, (Int,Int),(Int,Int))
rectACloseToTwoMil a = (a, last x, head y)
    where (x,y) = span (\(a,b) -> b < 2000000) $ rectA a

-- less than 16 away from 2000000 because (3, 816) already gets that close
filterClose :: [(Int,(Int,Int),(Int,Int))]
filterClose = filter (\(_,(_,a),(_,b)) -> 
                  abs (2000000-a) <= 16 || abs (2000000-a) <= 16) $
                  map rectACloseToTwoMil [1..1500]


main :: IO()
main = print filterClose 
-- gives rectangle of (36,77), area of 2772. Oddly very fast?
-- ah, there's a limit where even (1,x) will give over 2000000 rectangles
-- somewhere between 1500 and 2000.
    
    --print $ last $ takeWhile (\(a,b) -> b < 2000000) rectTwo
    --print $ head $ dropWhile (\(a,b) -> b < 2000000) rectTwo

