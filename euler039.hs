-- euler 39, integer right triangles
-- generate all right triangles whose perimeter is 
-- less than or equal to 1000.

-- which perimeter has the most solutions?

-- Don't worry about limit, go until c <= 1000

-- weird, adding in calcPerimeters after rightTriangles actually lowers the runtime
-- from 10 seconds to 5 seconds? Even weirder, calculating (a+b+c) directly inside
-- the list comprehension is still 10 seconds! as opposed to mapping it outside.

-- and rest of program is negligible. total about 6s

-- think it took me an hour, maybe 1.5, to figure it out. Mostly wrangling with
-- the histogram stuff

rightTriangles :: Int -> [(Int,Int,Int)]
rightTriangles limit = [(a,b,c)
                       | c <- [1..limit]
                       , b <- takeWhile (<c) [1..]
                       , a <- takeWhile(<b) [1..]
                       , a^2 + b^2 == c^2
                       , a+b+c <= limit]

-- returns a list of perimeters solns, non-sorted
calcPerimeters :: [(Int,Int,Int)] -> [Int]
calcPerimeters = map (\(a,b,c) -> a + b + c)

-- returns a histogram of perimeter solns, with index as the perimeter and 
-- the value as the count
histSol :: [Int] -> [Int]
histSol xs = map (\x -> length $ filter (==x) xs) [1..1000]

-- returns max of histo of perimeter solns, with tuple of (count, index)
getMax :: [Int] -> (Int,Int)
getMax xs = maximum $ zip xs [1..]

main :: IO()
main = print $ getMax $ histSol $ calcPerimeters $ rightTriangles 1000
