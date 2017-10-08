-- TEST REPORT
-- Vincent discovered the command ":set +s".
-- Using this command, ghci shows the execution time and
-- memory usage of function calls.

-- So, we run the program as follows:
-- :set +s
-- measure1
-- measure2

-- Performance and memory tested by generating 1000 sudoku problems with each implementation

-- Lecture5:
-- (15.77 secs, 7,279,371,872 bytes)

-- Ex2: (without the NRC constraint)
-- (55.62 secs, 18,111,723,848 bytes)

-- This shows the Ex2 implementation is significantly slower and uses a lot more memory.
-- This can be explained by the fact that "Constrnt" is a new 'layer' on top of the
-- existing "Constraint" in Lecture5, so more memory is needed. Also the 'elem'
-- function is used a lot, which increases the running time.

-- We also compared the solving of sudoku problems.
-- We are testing both versions by solving the given example sudokus from the lecture and checking their consistency.
-- Below we can see that the old version takes longer to execute but uses less resources than the refactored version.
-- *Ex2Test> runold
-- "5 tests passed"
-- (0.08 secs, 7,554,952 bytes)
-- *Ex2Test> runnew
-- "5 tests passed"
-- (0.05 secs, 9,677,176 bytes)

-- However, the execution time does not seem to be consistent. If we run old -> new then the results are as above.
-- If we reverse the order new -> old then the execution time of the refactored version is longer than the old version.
-- In terms of resources, the refactored version still consumes more regardless of order (also tested multiple times).

-- *Ex2Test> runnew
-- "5 tests passed"
-- (0.09 secs, 9,693,816 bytes)
-- *Ex2Test> runold
-- "5 tests passed"
-- (0.02 secs, 7,534,176 bytes)
-- *Ex2Test>

module Ex2Test where

import Data.List
import System.Random
import Lecture5
import Ex2
import Ex1

-- Measure Lecture5 performance on generating 1000 sudoku problems
generate100Ex1 :: Integer -> Integer -> IO ()
generate100Ex1 k n = if k == n then print "done "
                else do
                s <- Lecture5.genRandomSudoku
                p <- Lecture5.genProblem s
                generate100Ex1 (k + 1) n

measure1 = generate100Ex1 0 1000

-- Measure ex2 performance on generating 1000 sudoku problems
generate100Ex2 :: Integer -> Integer -> IO ()
generate100Ex2 k n = if k == n then print "done "
                else do
                s <- Ex2.genRandomSudoku
                p <- Ex2.genProblem s
                generate100Ex2 (k + 1) n

measure2 = generate100Ex2 0 1000

-- Solving sudoku problems

testPuzzles :: [Ex2.Grid]
testPuzzles = [Lecture5.example1] ++ [Lecture5.example2] ++ [Lecture5.example3] ++ [Lecture5.example4] ++ [Lecture5.example5]

isSudokuConsistent :: [Ex2.Node] -> Bool
isSudokuConsistent [] = True
isSudokuConsistent ((sudoku, constraints):nodes) = Ex2.consistent sudoku && isSudokuConsistent nodes

-- Test that the solved sudokus are still consistent (both with nrc constraint)

-- Solves the sudoku puzzles with the old code
solveSudoku1 k n (sud:suds) = if k == n then print (show n ++ " tests passed")
                else do
                  let xs = Ex1.solveNs (Ex1.initNode sud)
                  if isSudokuConsistent xs then
                       solveSudoku1 (k+1) n suds
                  else error ("failed test")

-- Solves the sudoku puzzles with the refactored code
solveSudoku2 k n (sud:suds) = if k == n then print (show n ++ " tests passed")
                else do
                  let xs = Ex2.solveNs (Ex2.initNode sud)
                  if isSudokuConsistent xs then
                       solveSudoku2 (k+1) n suds
                  else error ("failed test")

runold = solveSudoku1 1 (length testPuzzles) testPuzzles

runnew = solveSudoku2 1 (length testPuzzles) testPuzzles