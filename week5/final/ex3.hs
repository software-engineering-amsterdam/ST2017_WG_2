-- About an hour

-- Random sudoku problems are generated and
-- then we check if the problem has only one solution,
-- and all variants of the sudoku with 1 filled position
-- removed have more than 1 solution.

-- We tested with 100 examples and that passed,
-- but changed the code to only run 5 tests
-- so the waiting time is acceptable.

module Ex3 where

import Data.List
import System.Random
import Lecture5

-- First, get all filled positions of the sudoku.
-- Next, generate an array of all variants of the sudoku
-- with 1 filled position removed.
eraseHints :: Node -> [Node]
eraseHints s = [eraseN s n | n <- fp]
    where fp = filledPositions (fst s)

-- First, check if the original sudoku has 1 solution.
-- Next, check if all variants of the sudoku with 1 filled position
-- removed have more than 1 solution.
-- To do that, "take 2" is used as we don't need to know the exact number of solutions,
-- only if that number is more than 1.
-- Using take 2 saves a lot of time, because of lazy evaluation.
testMinimal :: Node -> Bool
testMinimal s = uniqueSol s && all (\x -> length (take 2 (solveNs [x])) == 2) e
    where e = eraseHints s

-- Own test function, using method from lecture 2 slides
testR :: Int -> Int -> (Node -> Bool) -> IO ()
testR k n f = if k == n then print (show n ++ " tests passed")
                else do
                s <- genRandomSudoku
                p <- genProblem s
                if f p then
                    testR (k+1) n f
                else do
                    print "failed test on:"
                    showNode p
                    error ("")

ownTest :: (Node -> Bool) -> IO ()
ownTest f = testR 1 5 f

main3 = do
    print "-- Start test --"
    ownTest testMinimal