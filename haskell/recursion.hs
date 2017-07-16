reverse_number :: Int -> Int -> Int
reverse_number n r | n < 10 = r * 10 + n
                   | n >= 10 = reverse_number (n `div` 10) ((10 * r) + (n `mod` 10))

sum_digit :: Int -> Int -> Int
sum_digit n r | n < 10 = r + n `mod` 10
              | n >= 10 = sum_digit (n `div` 10) (r + n `mod` 10)

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Writing length func
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- Writing sum func
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Recursive MDC - https://github.com/jhoonb/haskell-exemplos/blob/master/exemplos/mdc.hs
mdc :: Integral a => a -> a -> a
mdc m n | m `mod` n == 0 = n
        | m `mod` n /= 0 = mdc n (m `mod` n)

-- Find the maximum value in a list
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- Replicate an item n t times
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' t n
    | t < 1 = []
    | otherwise  = n:replicate' (t-1) n

-- Take n items from xs list
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

-- Reverse a list
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- Repeat a item
repeat' :: a -> [a]
repeat' x = x:repeat' x

-- zip two lists
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

-- Verify if a in xs
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = a `elem'` xs
