-- Project Euler problem 6

-- sum square difference

-- dif between sum of squares and square of sum

sumOfSquares :: Integral a => [a] -> a
sumOfSquares = sum . map (\n -> n * n) 

squareOfSum :: Integral a => [a] -> a
squareOfSum xs = sum' * sum'
    where sum' = sum xs

sumSquareDif :: Integral a => a -> a
sumSquareDif x = squareOfSum [1..x] - sumOfSquares [1..x]
