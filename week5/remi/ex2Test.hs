-- TEST REPORT
-- Shoutout to Vincent and Hasan for discovering the command ":set +s"

-- Performance and memory tested by generating 1000 sudoku problems with each implementation

-- Lecture5:
-- (15.77 secs, 7,279,371,872 bytes)

-- Ex2: (without the NRC constraint)
-- (55.62 secs, 18,111,723,848 bytes)

-- This shows the Ex2 implementation is significantly slower and uses a lot more memory.
-- This can be explained by the fact that "Constrnt" is a new 'layer' on top of the
-- existing "Constraint" in Lecture5, so more memory is needed.


module Ex2Test where

import Data.List
import System.Random
import Lecture5
import Ex2

-- Measure ex1 performance on generating 100 sudoku problems
generate100Ex1 :: Integer -> Integer -> IO ()
generate100Ex1 k n = if k == n then print "done "
                else do
                s <- Lecture5.genRandomSudoku
                p <- Lecture5.genProblem s
                generate100Ex1 (k + 1) n

measure1 = generate100Ex1 0 1000

-- Measure ex2 performance on generating 100 sudoku problems
generate100Ex2 :: Integer -> Integer -> IO ()
generate100Ex2 k n = if k == n then print "done "
                else do
                s <- Ex2.genRandomSudoku
                p <- Ex2.genProblem s
                generate100Ex2 (k + 1) n

measure2 = generate100Ex2 0 1000