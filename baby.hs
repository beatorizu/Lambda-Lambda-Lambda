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

-- Which right triangle that has integers for all sides and all sides equal to or smaller than 10 has a perimeter of 24?
triangules = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, sum [a,b,c] == 24 ]

removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

length' list = sum[1 | _ <-list]

doubleSmallNumber x = if x > 100 then x else doubleMe x

doubleSmallNumber' x = (if x > 100 then x else doubleMe x) + 1

doubleUs x y = doubleMe x + doubleMe y

doubleMe x = x + x

conanO'Brien = "It's a-me, Conan O'Brien!"

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
