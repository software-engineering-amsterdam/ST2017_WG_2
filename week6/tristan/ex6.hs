-- This took ~15 minutes
-- The test will never succeed (as observed when runnning this program).
-- Explanation is from https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test which states that:
--  > Its original version, due to Gary L. Miller, is deterministic,
--  > but the correctness relies on the unproven Extended Riemann hypothesis.
--  > Michael O. Rabin modified it to obtain an unconditional probabilistic algorithm.

module Ex6 () where

import Data.List
import Control.Monad
import Lecture6
import Ex3

findLeastFoolingInteger :: Int -> [Integer] -> IO Integer
findLeastFoolingInteger k (n:rest) = do
    fooled <- primeMR k n
    if fooled then
        return n
    else
        findLeastFoolingInteger k rest

main = do
    -- match1 <- findLeastFoolingInteger 1 composites
    -- match2 <- findLeastFoolingInteger 2 composites
    -- match3 <- findLeastFoolingInteger 3 composites
    -- match4 <- findLeastFoolingInteger 4 composites
    match5 <- findLeastFoolingInteger 10 composites

    -- print match1
    -- print match2
    -- print match3
    -- print match4
    print match5