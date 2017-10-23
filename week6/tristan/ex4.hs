-- This took ~45 minutes
--
-- Result from a random run:
-- 21
-- 91
-- 2465
-- 703
-- 2821
--
-- The less iterations the primality check uses, the less chance there is to find a suitable
-- prime. The algorithm has a tendency to stall when using k=1 and if we are unlucky.

module Ex4 (findLeastFoolingInteger) where

import Data.List
import Control.Monad
import Lecture6
import Ex3

findLeastFoolingInteger :: Int -> [Integer] -> IO Integer
findLeastFoolingInteger k (n:rest) = do
    fooled <- primeTestsF k n
    if fooled then
        return n
    else
        findLeastFoolingInteger k rest

main = do
    match1 <- findLeastFoolingInteger 1 composites
    match2 <- findLeastFoolingInteger 2 composites
    match3 <- findLeastFoolingInteger 3 composites
    match4 <- findLeastFoolingInteger 4 composites
    match5 <- findLeastFoolingInteger 5 composites

    print match1
    print match2
    print match3
    print match4
    print match5
