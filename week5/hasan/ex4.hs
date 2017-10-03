module Ex4 where

import Lecture5Original
import Data.List

blocksToRemove = 3
blocks_x = [[((r,c), 0) | r <- b1, c <- b2] | b1 <- blocks, b2 <- blocks ]
blocks_y = filter (\x -> length x == blocksToRemove) (subsequences blocks_x)
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
    showGrid (sud2grid s)
    let x = any (\positions -> uniqueSolHelper (removePositions s (concat positions))) blocks_y
    let y = filter (\positions -> uniqueSolHelper (removePositions s (concat positions))) blocks_y
    --print "has uniq without removed blocks: "
    --print x
    let positions = head y
    let s' = removePositions s (concat positions)
    showGrid (sud2grid s')
    print "Has unique solution:"
    print (uniqueSolHelper s')