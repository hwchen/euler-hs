-- number spirals

-- sum of diagonals on a number spiral for edge length = 1001.

-- 1, [3,5,7,9], [13,17,21,25], [31,37,43,49]

-- only works for odd numbers!
diagonalsOfEdge :: Int -> [Int]
diagonalsOfEdge 1 = [1]
diagonalsOfEdge n = spiralDiags ++ (take 4 $ drop 1 $ iterate (+ (n - 1)) (last spiralDiags))
    where spiralDiags = diagonalsOfEdge (n-2)

main = print $ sum $ diagonalsOfEdge 1001
