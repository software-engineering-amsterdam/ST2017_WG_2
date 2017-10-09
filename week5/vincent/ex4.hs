module Ex4 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture5

blockPos :: [[((Row, Column), Value)]]
blockPos = [ [((r, c), 0) | r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks]

randomBlock :: IO [((Row, Column), Value)]
randomBlock = do
    randPos <- getRandomInt 8
    return (blockPos !! randPos)

makeEmpty :: Sudoku -> [((Row, Column), Value)] -> Sudoku
makeEmpty s [] = s
makeEmpty s (((r,c),v):rest) = makeEmpty (extend s ((r,c),v)) rest

removeBlocks :: Int -> Sudoku -> IO Sudoku
removeBlocks 0 sud = return sud
removeBlocks n sud = do
    randBlock <- randomBlock
    removeBlocks (n-1) (makeEmpty sud randBlock)

genEmpty3 :: IO Node
genEmpty3 = do 
    [(n,cs)] <- rsolveNs [emptyN]
    m <- removeBlocks 3 n
    let xs = filledPositions m
    ys <- randomize xs
    return (minimalize (m,cs) ys) 

main = do
    p <- genEmpty3
    showNode p