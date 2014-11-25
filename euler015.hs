-- euler 15.
-- lattice path

--ah, all you have to do is increase one or the other of (x,y). So, I can just
-- generate all lists of all coordinates where x or y is increasing.

-- Hard to generate paths... I'm just doing it numerically. It's pascal's triangle.
-- generate row i+j (0 is top row) where i and j are # of rows and columns in grid. Then,
-- find the ith number (indexing from 0) of that row. Here, I was lazy and just got the
-- maximum of that row.

nextPascalRow :: Integral a => [a] -> [a]
nextPascalRow row = go row'
    where go [] = []
          go [x] = [x]
          go (x:y:xs) = x+y : go (y:xs)
          row' = 0 : row 

pascalTriangleRow :: Integral a => a -> [a]
pascalTriangleRow 0 = [1]
pascalTriangleRow n = nextPascalRow $ pascalTriangleRow (n-1) 

main = print maximum $ pascalTriangleRow 40

-- it's fast enough to just map to row# to create full pascal triangle.
-- but I could probably figure out a way to generate it without redoing all
-- the recursive calls for each row.
