-- euler 16
-- power digit sum

-- converting each char to string is expensive?
pwrDigitSum :: Integer -> Integer
pwrDigitSum n = sum $ map (\x -> read [x] :: Integer) (show (2^n))
