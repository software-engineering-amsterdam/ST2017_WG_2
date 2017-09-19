-- ID: 11408227
-- Name: Vincent Jong
-- Time: 14:20 - 15:00 (Programming), (Testing), (Answering)

-- Testing by creating tuples (String, Form) in which the 1st arg is the input to the parse
-- function and 2nd arg is the expected output. The result of parse and the 2nd arg will be 
-- compared for equiality

module Ex2 where

import Data.List
import Test.QuickCheck
import System.Random
import Lecture3

testInput :: [(String, Form)]
testInput = [
                ("*(1 +(2 3))", (Cnj [p, (Dsj[q, r])]))
            ]

doTest :: [(String, Form)] -> Bool
doTest [] = True
doTest (x:xs) = res == exp && doTest xs
    where 
        res = head(parse (fst x))
        exp = snd x

main = do
    print $ doTest testInput