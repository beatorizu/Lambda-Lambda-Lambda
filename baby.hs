length' list = sum[1 | _ <-list]

doubleSmallNumber x = if x > 100 then x else doubleMe x

doubleSmallNumber' x = (if x > 100 then x else doubleMe x) + 1

doubleUs x y = doubleMe x + doubleMe y

doubleMe x = x + x

conanO'Brien = "It's a-me, Conan O'Brien!"

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
