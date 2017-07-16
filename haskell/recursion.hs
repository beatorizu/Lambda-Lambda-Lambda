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
