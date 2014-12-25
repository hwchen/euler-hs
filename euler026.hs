-- reciprocal cycles
-- euler 26

-- find value d for d < 1000 which has the longest cycle for 1/d

-- generating this way is not long enough to show long cycles.
genReals :: [String] 
genReals = [show (1/d) | d <- [1..999]]

-- also remove last digit, since that's often rounded
cleanRealString :: String -> String
cleanRealString s = init $ removeE $ removeDot s
    where removeDot = filter (`notElem` ".")
          removeE s' 
              | 'e' `elem` s' = init $ init $ init $ s'
              | otherwise = s'

cleanReals :: [String]
cleanReals = map cleanRealString genReals
