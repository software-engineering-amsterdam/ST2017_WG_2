-- About 5 minutes

module Ex3 where

import Data.List
import System.Random
import Lecture6

-- Composite numbers are not prime, so generate list of
-- non-prime numbers starting at 4
composites :: [Integer]
composites = 4 : filter (\x -> not (prime x)) [6..]