-- About an hour

-- Testing using a list of tuples containing first the expression
-- to be parsed, and second the expected output of the parse function.
-- Next, it is tested by a function that tests if the parsed
-- outcome is equal to the expected outcome.

module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- Test by parsing, then comparing the output to the expected output from the list
parseTest :: [(String, Form)] -> Bool
parseTest = all (\(x, y) -> parse x == [y])

-- List of test cases and expected parsing output
testList :: [(String, Form)]
testList =  [
                ("*(1 +(2 -3))", Cnj [p, Dsj [q, Neg r]]),
                ("*(1 +(2 -3))))", Cnj [p, Dsj [q, Neg r]]),
                ("*(+(2 -3) 1)", Cnj [Dsj [q, Neg r], p]),
                ("+(1 +(2 -3))", Dsj [p, Dsj [q, Neg r]]),
                ("+(+(2 -3) 1)", Dsj [Dsj [q, Neg r], p]),
                ("*(1 +(2 3))", Cnj [p, Dsj [q, r]]),
                ("*(+(2 3) 1)", Cnj [Dsj [q, r], p]),
                ("+(1 *(2 3))", Dsj [p, Cnj [q, r]]),
                ("+(*(2 3) 1)", Dsj [Cnj [q, r], p]),
                ("(+(1 2) ==> 3)", Impl (Dsj [p, q]) r),
                ("(3 ==> +(1 2))", Impl r (Dsj [p, q])),
                ("(+(1 2) <=> 3)", Equiv (Dsj [p, q]) r),
                ("(3 <=> +(1 2))", Equiv r (Dsj [p, q])),
                ("(-3 <=> +(1 2))", Equiv (Neg r) (Dsj [p, q]))
            ]

main = parseTest testList