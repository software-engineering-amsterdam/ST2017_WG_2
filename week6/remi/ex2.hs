-- About 45 minutes

-- Tested with 1000 random cases, using :set +s

-- exM
-- (0.12 secs, 10,305,224 bytes)

-- expM
-- (3.98 secs, 178,177,856 bytes)

-- This clearly shows the improvement in time and memory usage of exM compared to expM

module Ex2 where

import Data.List
import System.Random
import Ex1
import Lecture6

-- Measure ex1 performance on generating 100 test cases
testEx1 :: Integer -> Integer -> IO ()
testEx1 k n = if k == n then print "done "
                else do
                x <- getStdRandom (randomR (1,65000))
                y <- getStdRandom (randomR (1,65000))
                z <- getStdRandom (randomR (1,65000))
                let aa = Ex1.exM x y z
                if aa >= 0 then testEx1 (k + 1) n else error "<0"

testLecture6 :: Integer -> Integer -> IO ()
testLecture6 k n = if k == n then print "done "
                else do
                x <- getStdRandom (randomR (1,65000))
                y <- getStdRandom (randomR (1,65000))
                z <- getStdRandom (randomR (1,65000))
                let aa = Lecture6.expM x y z
                if aa >= 0 then testLecture6 (k + 1) n else error "<0"

measure1 = testEx1 0 1000

measure2 = testLecture6 0 1000