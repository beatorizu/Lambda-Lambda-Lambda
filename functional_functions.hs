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
reverse_number 1 r = r * 10 + 1
reverse_number 2 r = r * 10 + 2
reverse_number 3 r = r * 10 + 3
reverse_number 4 r = r * 10 + 4
reverse_number 5 r = r * 10 + 5
reverse_number 6 r = r * 10 + 6
reverse_number 7 r = r * 10 + 7
reverse_number 8 r = r * 10 + 8
reverse_number 9 r = r * 10 + 9
reverse_number n r = reverse_number (n `div` 10) ((10 * r) + (n `mod` 10))

sum_digit :: Int -> Int -> Int
sum_digit 1 r = r + 1 `mod` 10
sum_digit 2 r = r + 2 `mod` 10
sum_digit 3 r = r + 3 `mod` 10
sum_digit 4 r = r + 4 `mod` 10
sum_digit 5 r = r + 5 `mod` 10
sum_digit 6 r = r + 6 `mod` 10
sum_digit 7 r = r + 7 `mod` 10
sum_digit 8 r = r + 8 `mod` 10
sum_digit 9 r = r + 9 `mod` 10
sum_digit n r = sum_digit (n `div` 10) (r + n `mod` 10)

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)
