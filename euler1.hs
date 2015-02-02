-- euler 1

-- sum of all multiples below 1000

import Data.List

fizzBuzz :: Int -> [Int]
fizzBuzz limit = filter (\x -> x `rem` 3 == 0 || x `rem` 5 == 0) [1..limit-1]

main :: IO()
main = print $ sum $ fizzBuzz 1000
