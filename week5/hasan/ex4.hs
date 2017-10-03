-- sudoku's with 3 blocks removed can have unique solutions
-- sudoku's with 4 blocks removed can have unique solutions
-- sudoku's with 5 blocks removed cannot have unique solutions

module Ex4 where

import Lecture5Original
import Data.List

blocks_x = [[((r,c), 0) | r <- b1, c <- b2] | b1 <- blocks, b2 <- blocks ]
blocks_y3 = filter (\x -> length x == 3) (subsequences blocks_x)
blocks_y4 = filter (\x -> length x == 4) (subsequences blocks_x)
uniqueSolHelper :: Sudoku -> Bool
uniqueSolHelper s = uniqueSol (s, constraints s)

removePositions :: Sudoku -> [((Row,Column), Value)] -> Sudoku
removePositions s [] = s
removePositions s (((r,c),v):ps) = removePositions (extend s ((r,c),v)) ps

getSudoku :: Maybe Sudoku -> Sudoku
getSudoku s = case s of 
    Just sx -> sx
    otherwise -> undefined

runEx4 = do
    (s, cs) <- genRandomSudoku
    --showGrid (sud2grid s)
    let x = any (\p -> uniqueSolHelper (removePositions s (concat p))) blocks_y3
    let y = filter (\p -> uniqueSolHelper (removePositions s (concat p))) blocks_y3
    let positions = head y
    let s' = removePositions s (concat positions)
    showGrid (sud2grid s')
    print "Has unique solution:"
    print (uniqueSolHelper s')
    
    let x2 = any (\p -> uniqueSolHelper (removePositions s (concat p))) blocks_y4
    let y2 = filter (\p -> uniqueSolHelper (removePositions s (concat p))) blocks_y4
    let positions2 = head y2
    let s2' = removePositions s (concat positions2)
    showGrid (sud2grid s2')
    print "Has unique solution:"
    print (uniqueSolHelper s2')