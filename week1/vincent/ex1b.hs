-- ID: 11408227
-- Name: Vincent Jong
-- Time: ~20 min

import Test.QuickCheck

exprleft :: Int -> Int
exprleft x = if x <= 1 then 1 else x * x * x + exprleft (x - 1)

exprright :: Int -> Int
exprright x = if x <= 1 then 1 else (x * (x + 1) `div` 2) ^ 2

exer3 :: Int -> Bool
exer3 x = exprleft (x) == exprright (x)

main = quickCheck exer3