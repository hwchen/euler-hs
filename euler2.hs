-- euler 2
-- even fib numbers under 4 million

fibTo :: Int -> [Int]
fibTo limit = takeWhile (< limit) $map (\(a,_) -> a) $ iterate (\(a,b) -> (b, a+b)) (1,1)

main :: IO()
main = print $ sum $ filter (even) $ fibTo 4000000
