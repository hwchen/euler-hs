--euler 14
--longest collatz sequence with seed under 1 million
-- 8 seconds
import Data.List

collatzSeq :: Integral a => a -> [a]
collatzSeq 1 = [1]
collatzSeq n
    | n `rem` 2 == 0 = n : collatzSeq (n `div` 2)
    | otherwise = n: collatzSeq (3*n + 1)


collatzLengthsUpto ::  Int -> [(Int,Int)]
collatzLengthsUpto n = map (\x -> (x, length $ collatzSeq x)) [1..n]

maximumReturnIndexValue :: [(Int,Int)] -> (Int,Int)
maximumReturnIndexValue = maximumBy cmp
    where cmp (_, x) (_, y) = compare x y

main = print $ fst $ maximumReturnIndexValue $ collatzLengthsUpto 1000000
