-- Time: about 2 hrs

-- The implementation with squaring modulo each time has a major impact on performance.
-- Below we can see that it is more efficient than rem (x ^ y)

-- *Ex2> runold
-- "Done 1000 tests"
-- (0.23 secs, 28,826,344 bytes)
-- *Ex2> runnew
-- "Done 1000 tests"
-- (0.03 secs, 13,404,016 bytes)
-- *Ex2>

module Ex2 where

import Data.List
import System.Random
import Lecture6
import Ex1

testExpm = do
    print $ Lecture6.expM 5 117 19 -- Answer = 1
    print $ Lecture6.expM 4 13 497 -- Answer = 445
    print $ Lecture6.expM 7 256 13 -- Answer = 9

testExm = do
    print $ Ex1.exM 5 117 19 -- Answer = 1
    print $ Ex1.exM 4 13 497 -- Answer = 445
    print $ Ex1.exM 7 256 13 -- Answer = 9

testRandExpm :: Int -> Int -> IO ()
testRandExpm k n = if k == n then print ("Done " ++ (show k) ++ " tests")
                    else do
                    x <- getStdRandom (randomR (1, 10000))
                    y <- getStdRandom (randomR (1, 10000))
                    z <- getStdRandom (randomR (1, 10000))
                    let r = Lecture6.expM x y z
                    if r >= 0 then testRandExpm (k+1) n else error ""


testRandExm :: Int -> Int -> IO ()
testRandExm k n = if k == n then print ("Done " ++ (show k) ++ " tests")
                    else do
                    x <- getStdRandom (randomR (1, 10000))
                    y <- getStdRandom (randomR (1, 10000))
                    z <- getStdRandom (randomR (1, 10000))
                    let r = Ex1.exM x y z
                    if r >= 0 then testRandExm (k+1) n else error ""

runold = testRandExpm 1 1000

runnew = testRandExm 1 1000
