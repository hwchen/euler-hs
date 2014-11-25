-- euler 24
-- ordered permutations.
-- millionth permutation of 0,1,2,3,4,5,6,7,8,9

-- permutations are like doing sorts?
-- reverse list, switch head, then reverse again. Then repeat

-- or generate: list comprehension
import Data.List

perms' = [x++y++z | x <- xs, y <- xs, z <- xs, x /= y, y /= z, x /= z]
    where xs = ["0","1","2"]

--sigh, finally looked up permutations function in hoogle. Pretty complicated.
-- list comprehensions definitely not the way to go.

-- oops, can't use prelude permutations because it's not in lexicographic order.
-- and full set of permutations is too large for memory. I'll come back to it.

main = print $ snd $ head $ dropWhile (\(x,y) -> x < 1000000 ) $ 
       zip [1..] $ sort $ permutations ['0','1','2','3','4','5','6','7','8','9']

