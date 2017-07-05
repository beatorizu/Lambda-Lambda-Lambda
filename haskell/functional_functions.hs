-- Pattern matching
sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

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

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Take fst, snd and trd from triple :3
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- Writing head func
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

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

capital :: String -> String
capital "" = "Empty string, whoops!"
capital t@(x:xs) = "The first letter of " ++ t ++ " is " ++ [x]

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y
