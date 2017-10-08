-- In exercise 1 we already implemented NRC constraints which allowed us to generate NRC sudokus.
-- Therefore we can simply import the Lecture5 module and use genRandomSudoku

module Ex5 where

import Lecture5

runEx5 = do
    (s, c) <- genRandomSudoku
    showGrid (sud2grid s)