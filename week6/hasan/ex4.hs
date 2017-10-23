module Ex4 where

import Ex3
import Lecture6
import Data.List
import Control.Monad

check1 :: [Integer] -> IO Integer
check1 (c:cc) = do
    res <- primeTestsF 1 c
    if res then return c else do (check1 cc)

main = do
    res <- check1 compositesCustom
    print res