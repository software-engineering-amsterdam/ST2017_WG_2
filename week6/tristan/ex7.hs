module Ex7 () where

import Debug.Trace
import Control.Monad
import Lecture6

mersennePrimes :: [Integer] -> [IO Integer]
mersennePrimes (n:ps) = do
    isMatch <- primeMR 10 ((2^n)-1)
    if isMatch then do
        next <- [mersennePrimes ps]
        (return n) : next
    else
        mersennePrimes ps

-- main = mapM print mersennePrimes
