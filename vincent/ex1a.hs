-- ID: 11408227
-- Name: Vincent Jong
-- Time: ~ 30min

import Test.QuickCheck

exprleft :: Int -> Int
exprleft x = if x <= 1 then 1 else x * x + exprleft (x - 1)

exprright :: Int -> Int
exprright x = if x <= 1 then 1 else (x * (x + 1) * (2 * x + 1)) `div` 6

exer2 :: Int -> Bool
exer2 x = exprleft (x) == exprright (x)

main = quickCheck exer2