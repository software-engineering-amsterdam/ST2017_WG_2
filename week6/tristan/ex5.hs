-- This took ~10 minutes.
--
-- Result is most of the time: 294409.
-- Explanation from https://en.wikipedia.org/wiki/Carmichael_number:
-- A Carmichael number will pass a Fermat primality test to every base b relatively prime to the number, even though it is not actually prime.
-- Relatively prime is when two numbers share no common positive factors except 1.

module Ex5 () where

import Ex4
import Lecture6

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
    k <- [2..], 
    prime (6*k+1), 
    prime (12*k+1), 
    prime (18*k+1) ]

main = do
    match1 <- findLeastFoolingInteger 1 carmichael
    match2 <- findLeastFoolingInteger 2 carmichael
    match3 <- findLeastFoolingInteger 3 carmichael
    match4 <- findLeastFoolingInteger 4 carmichael
    match5 <- findLeastFoolingInteger 5 carmichael

    print match1
    print match2
    print match3
    print match4
    print match5