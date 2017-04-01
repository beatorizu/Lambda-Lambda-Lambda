-- Bricks queue
brick_queue :: Int -> Int -> Int -> Bool
brick_queue small_brick big_brick queue_length = if small_brick * 1 + big_brick * 5 >= queue_length then True else False
-- Test case
-- brick_queue 3 1 8 == True
-- brick_queue 3 1 9 == False
-- brick_queue 3 2 10 == True
-- brick_queue 3 2 8 == True
-- brick_queue 3 2 9 == True
-- brick_queue 6 1 11 == True
-- brick_queue 6 0 11 == False
-- brick_queue 3 1 7 == True
-- brick_queue 1 1 7 == False
-- brick_queue 2 1 7 == True
-- brick_queue 7 1 11 == True
-- brick_queue 7 1 8 == True
-- brick_queue 7 1 13 == False
-- brick_queue 43 1 46 == True
-- brick_queue 40 1 46 == False
-- brick_queue 22 2 33 == False
-- brick_queue 0 2 10 == True
-- brick_queue 1000000 1000 1000100 == True
-- brick_queue 2 1000000 100003 == True
-- brick_queue 12 2 21 == True

-- Dist10
dist10 :: Int -> Bool
dist10 n | abs(200 - n) < abs(100 - n) = if abs(200 - n) <= 10 then True else False
         | abs(100 - n) < abs(200 - n) = if abs(100 - n) <= 10 then True else False
-- Test case
-- dist10 93 == True
-- dist10 90 == True
-- dist10 89 == False
-- dist10 110 == True
-- dist10 111 == False
-- dist10 121 == False
-- dist10 0 == False
-- dist10 5 == False
-- dist10 191 == True
-- dist10 189 == False
-- dist10 190 == True
-- dist10 200 == True
-- dist10 210 == True
-- dist10 211 == False
-- dist10 290 == False
