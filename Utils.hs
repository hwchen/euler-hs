module Utils (isPrime, sieveSundaram, sieveErast, squaresList, cartProd, cartProd3) where

squaresList :: Integral a => [a]
squaresList = map (\x -> x*x) [1..]

-- fun, but is slow because checks every single number in list
isPrime' :: Integral a => a -> Bool
isPrime' 1 = False 
isPrime' n = foldr (&&) True $ map (\x -> n `rem` x /= 0) [2..n-1]

isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime n = go 2
    where go x 
            | x * x > n = True 
            | n `rem` x == 0 = False 
            | otherwise = go (x+1)

--sieve
sieveSundaram :: Integral a => a -> [a]
sieveSundaram n = map (\n -> 2*n+1) $ filter (`notElem` filterList) [1..limit]
    where numList = cartProd [1..limit] [1..limit]
          filterList = map (\(x,y) -> (x+y+(2*x*y))) numList
          limit = 2*n+2

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

cartProd3 :: [a] -> [b] -> [c] -> [(a,b,c)]
cartProd3 xs ys zs = [(x,y,z) | x <- xs, y <- ys, z <- zs]

--sieveErastosthenes
sieveErast :: Integral a => a -> [a]
sieveErast n = sieveErast' n [1..n]

sieveErast' :: Integral a => a -> [a] -> [a]
sieveErast' 1 xs = tail xs
sieveErast' n xs
    | isPrime n = sieveErast' (n-1) $ (filter (\x -> x == n || x `rem` n /= 0) xs)
    | otherwise = sieveErast' (n-1) xs
