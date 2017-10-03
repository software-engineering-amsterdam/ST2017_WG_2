-- About 30 minutes (a+b)

-- As it takes quite long, the code currently takes only 2 samples
-- However, we tested with 100 samples.
-- Average over 100 samples (NRC sudoku): 16.85

module Ex7 where

import Data.List
import System.Random
import Ex1

countNonEmpty :: Node -> Integer
countNonEmpty s = genericLength (filledPositions (fst s))

getAverageFilled :: Integer -> Integer -> Integer -> IO ()
getAverageFilled k n total = if k == n then print (fromIntegral total / fromIntegral n)
                else do
                s <- genRandomSudoku
                p <- genProblem s
                getAverageFilled (k + 1) n (total + countNonEmpty p)

main7 = getAverageFilled 0 2 0