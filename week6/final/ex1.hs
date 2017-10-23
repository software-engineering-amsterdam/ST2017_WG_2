-- This took 30 minutes
-- We have chosen for this solution because all team members had similar solutions but this one was the most simple.

module Ex1 (exMEfficient) where

import Data.Bits
import Lecture6

-- Implemented from the Right-to-left binary method pseudocode from https://en.wikipedia.org/wiki/Modular_exponentiation
-- The original comes from Applied Cryptography by Bruce Schneier.
exMEfficient :: Integer -> Integer -> Integer -> Integer
exMEfficient _ _ 1 = 0
exMEfficient base expo modulus = exMEfficientStep (base `mod` modulus) expo modulus 1

exMEfficientStep :: Integer -> Integer -> Integer -> Integer -> Integer
exMEfficientStep base expo modulus c
    | expo > 0 = exMEfficientStep ((base * base) `mod` modulus) (expo `shift` (-1)) modulus nextC
    | otherwise = c
    where nextC = if expo `mod` 2 == 1 then (c * base) `mod` modulus else c

main = do
    print (expM 98328 168277 17982571)
    print (exMEfficient 98328 168277 17982571)