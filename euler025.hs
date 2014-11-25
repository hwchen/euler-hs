-- euler 25
-- first fib number with 1000 digits

fib :: [Integer]
fib = 1 : 1 : [x+y | (x,y) <- zip fib (tail fib)]

firstFibAbove :: Int -> Int
firstFibAbove n = fst $ head $ dropWhile (\(x,y) -> (length $ show y) < n) (zip [1..] fib)

main = print $ firstFibAbove 1000
