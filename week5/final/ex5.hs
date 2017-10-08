-- In exercise 1 we already implemented NRC constraints which allowed us to generate NRC sudokus.
-- Therefore we can simply import the Ex1 module and use genRandomSudoku

module Ex5 where

import Ex1

main5 = do
    r <- genRandomSudoku
    showNode r
    s  <- genProblem r
    showNode s