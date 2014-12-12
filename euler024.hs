-- euler 24
-- ordered permutations.
-- millionth permutation of 0,1,2,3,4,5,6,7,8,9

import Data.List

-- oops, can't use prelude permutations because it's not in lexicographic order.
-- and full set of permutations is too large for memory. I'll come back to it.

-- there are 10! = 3628800. To check up # million: each "head" number has 1/10
-- of total permutations. millionth is between 2 and 3. So just check
-- permutations of 0,1,2,4,5,6,7,8,9 without 3 and add 3 back at end.

-- 2 starts at 725760 + 1. so millionth will be 1,000,000 - 725760 -1 index 
-- of permutations of 2. is this right? No, don't need off by one.
-- Just eliminate all '0' and '1', then it will be # 274240 (starting at 1)

main = print $ ('2' : (head $ drop 274239 $ sort $ permutations
                ['0','1','3','4','5','6','7','8','9']))


