-- Time: about 1 hr

-- Running with k = 1,2,3 always returned 294409 which is not a prime (divisible by 37) or 56052361 (divisible by 211)
-- *Ex5> main
-- 294409
-- 294409
-- 294409
-- 56052361
-- (0.03 secs, 765,800 bytes)
-- *Ex5> main
-- 56052361
-- 294409
-- 294409
-- 294409
-- (0.02 secs, 310,648 bytes)
-- *Ex5> main
-- 294409
-- 294409
-- 294409
-- 294409
-- (0.00 secs, 287,608 bytes)
-- *Ex5> main
-- 294409
-- 294409
-- 294409
-- 294409
-- (0.00 secs, 287,608 bytes)
-- *Ex5> main
-- 294409
-- 294409
-- 294409
-- 294409
-- (0.02 secs, 287,640 bytes)
-- *Ex5> main
-- 294409
-- 294409
-- 294409
-- 294409
-- (0.00 secs, 287,624 bytes)

-- So we can see that Fermat's check can be fooled by carmichael numbers

module Ex5 (carmichael) where

import Data.List
import System.Random
import Lecture6
import Ex4

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
    k <- [2..], 
    prime (6*k+1), 
    prime (12*k+1), 
    prime (18*k+1) ]

testCarmichael :: Int -> IO Integer
testCarmichael k = doPrimeTest carmichael k

main = do
    let k = 1
    r <- testCarmichael k
    print r 
    let k2 = 2
    r2 <- testCarmichael k2
    print r2
    let k3 = 3
    r3 <- testCarmichael k3
    print r3
    let k4 = 5
    r4 <- testCarmichael k4
    print r4