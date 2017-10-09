-- Since exercise 1 already generates random sudokus with the NRC constraint we can call the generator and
-- create the puzzle from it.

module Ex5 where

import Ex1

main = do
    ransudoku <- genRandomSudoku
    showNode ransudoku
    problem <- genProblem ransudoku
    showNode problem