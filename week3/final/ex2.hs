-- About an hour

-- Test 1 using the form generator from exercise 4
-- First, the form is turned into the string representation
-- Next, this string representation is parsed and then again
-- turned into the string representation. This function
-- checks if the result is equal to the original.

-- Test 2 using a list of tuples containing first the expression
-- to be parsed, and second the expected output of the parse function.
-- Next, it is tested by a function that tests if the parsed
-- outcome is equal to the expected outcome. This way,
-- also cases with too many ending brackets are tested.

module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Ex4

testGenerated :: Form -> Bool
testGenerated x = formString == showLst (parse formString)
    where formString = showLst [x]

-- Test by parsing, then comparing the output to the expected output from the list
-- This way, also cases with too many closing brackets are tested
testExamples :: [(String, Form)] -> Bool
testExamples = all (\(x, y) -> parse x == [y])

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

main = do
    quickCheck testGenerated
    print (testExamples testList)