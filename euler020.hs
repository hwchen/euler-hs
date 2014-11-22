-- euler 20
-- find sum of digits of factorial 100
import Utils (intToDigitList)

fac n = product [1..n]

main = print $ sum $ intToDigitList $ fac 100
