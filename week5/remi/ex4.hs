-- Started 10:30

-- This approach randomly clears n blocks and tries to minimalize
-- the result. If possible, a minimal sudoku is returned.
-- If not possible, a non-minimal sudoku with n empty blocks is returned.

-- It is possible to generate minimal sudoku's with up to 4 empty blocks.
-- Proof:  https://puzzling.stackexchange.com/a/319

-- An example of a minimal sudoku with 4 empty blocks is:
-- +-------+-------+-------+
-- |       |       | 2 7   |
-- |       |       | 4   1 |
-- |       |       | 5 8 9 |
-- +-------+-------+-------+
-- | 9     | 4 8 3 |       |
-- |     3 | 5     |       |
-- | 1 8 4 | 9     |       |
-- +-------+-------+-------+
-- | 2     | 6   9 |       |
-- | 4   9 | 2 3 5 |       |
-- |   3 1 |     4 |       |
-- +-------+-------+-------+

module Ex4 where

import Data.List
import System.Random
import Lecture5
import Ex3

-- Erase list of (Row,Column)-pairs from Node
eraseNl :: Node -> [(Row,Column)] -> Node
eraseNl n [] = n
eraseNl n ((r,c):xs) = eraseNl (eraseN n (r,c)) xs

-- Erase block i from Node
eraseBlock :: Node -> Integer -> Node
eraseBlock n i = eraseNl n block
    where rows = blocks !! (floor (fromIntegral i / 3.0))
          cols = blocks !! (fromIntegral (i `mod` 3))
          block = [(x, y) | x <- rows, y <- cols]

-- Erase n blocks from Node
eraseBlocks :: Node -> [Integer] -> Node
eraseBlocks n [] = n
eraseBlocks n (i:is) = eraseBlocks (eraseBlock n i) is

-- Generate problem with n randomly erased blocks
emptyBlockProblem :: Int -> IO Node
emptyBlockProblem n = do
    r <- genRandomSudoku
    b <- randomize [0..8]
    return (eraseBlocks r (take n b))

-- Generate minimal problem with n erased blocks
minimalEmptyBlockProblem :: Int -> IO Node
minimalEmptyBlockProblem n = do
    e <- emptyBlockProblem n
    genProblem e

main4 = do
    p <- minimalEmptyBlockProblem 5
    showNode p
    print (testMinimal p)