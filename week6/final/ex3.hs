-- This took 5 minutes
-- We have chosen for this solution because all team members had similar solutions but this one was the most simple.

module Ex3 where

import Lecture6

composites :: [Integer]
composites = [x | x <- [4..], not (prime x)]

main = do
    print Ex3.composites