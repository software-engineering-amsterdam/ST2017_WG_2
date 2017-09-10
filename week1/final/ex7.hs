-- Time taken: 3 hours
-- Sources:
-- VISA specification source: http://www.regular-expressions.info/creditcard.html
-- https://stackoverflow.com/questions/1918486/convert-list-of-integers-into-one-int-like-concat-in-haskell
-- Luhn algorithm linked in lecture: https://en.wikipedia.org/wiki/Luhn_algorithm
-- To test, we used credit card numbers known to be correct/incorrect.
-- We chose not to use an own generator of credit card numbers, because that generator should
-- be tested as well, by our Luhn validator which is not tested yet, and so forth...
-- Generate random credit card numbers: http://www.getcreditcardnumbers.com/

-- Import libraries
import Test.QuickCheck
import Data.Char
import Data.List

-- The main luhn function that will be called, converts number into digit array and calls helper function
luhn :: Integer -> Bool
luhn n = luhnHelper (getDigits (show n))

-- Checks weather if the sum of 3rd row luhn numbers sum to a number where n rem 10 == 0 --
luhnHelper :: [Integer] -> Bool
luhnHelper n = (sum(luhn2ndTo3rdRow (reverse (luhn1stTo2ndRow (reverse n) 0))) `rem` 10) == 0

-- Converts luhn row 1 to luhn row by doubling every second number
luhn1stTo2ndRow :: [Integer] -> Integer -> [Integer]
luhn1stTo2ndRow [] pos = []
luhn1stTo2ndRow n pos = if pos `rem` 2 == 0 then [head(n)] ++ (luhn1stTo2ndRow (tail n) (pos+1)) else [(head(n)*2)] ++ (luhn1stTo2ndRow (tail n) (pos+1))

-- Converts luhn row 2 to luhn row 2 by summing the digits of every number greater than 9, so 16 becomes 1 + 6 = 7
luhn2ndTo3rdRow :: [Integer] -> [Integer]
luhn2ndTo3rdRow [] = []
luhn2ndTo3rdRow n = if head(n) < 10 then [head(n)] ++ luhn2ndTo3rdRow(tail n) else [sum(getDigitsHelper(head n))] ++ luhn2ndTo3rdRow(tail n)

-- Convert a string representing a number to an array or int numbers, for example "123" to [1, 2, 3]
getDigitsHelper :: Integer -> [Integer]
getDigitsHelper n = getDigits (show n)
getDigits :: String -> [Integer]
getDigits n = if length(n) == 0 then [] else [toInteger(digitToInt (head n))] ++ getDigits(tail(n))

-- Convert int array like [1, 2, 3] to 123
intArrayToInt :: [Integer] -> Integer -> Integer
intArrayToInt [] pos = 0
intArrayToInt n pos = (10^pos * head(n)) + (intArrayToInt (tail n) (pos+1))

-----------------------
-- VISA CHECKS
isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n = isAmericanExpressHelper (getDigits (show n)) && luhn(n)
isMaster n = isMasterHelper (getDigits (show n)) && luhn(n)
isVisa n = isVisaHelper (getDigits (show n)) && luhn(n)
isAmericanExpressHelper n = length(n) == 15 && head(n) == 3 && (head(tail(n)) == 4 || head(tail(n)) == 7)
isMasterHelper n =
    length(n) == 16 &&
    (
        (intArrayToInt (reverse (take 2 n)) 0) == 51 ||
        (intArrayToInt (reverse (take 2 n)) 0) == 55 ||
        (
            (intArrayToInt (reverse (take 4 n)) 0) >= 2221 &&
            (intArrayToInt (reverse (take 4 n)) 0) <= 2720
        )
    )
isVisaHelper n = (length(n) == 16 || length(n) == 13) && head(n) == 4


-- Test to check if a cardnumber belongs to any of the 3 cards as expected
-- input: <cardnumber> <isVisa> <isMaster> <isAmericanExpress>, output: true if the algorithm returns the same values as the input booleans, false otherwise
creditCardTest :: Integer -> Bool -> Bool -> Bool -> Bool
creditCardTest n a b c = (isVisa n) == a && (isMaster n) == b && (isAmericanExpress n) == c

-- Test a few cards, they all must return True
main = do
    print (creditCardTest 1 False False False) -- Bad card number (length fails for all cards)
    print (creditCardTest 4556641618423 True False False) -- Visa card
    print (creditCardTest 455664161842 False False False) -- Bad Visa card (length fails)
    print (creditCardTest 3556641618423 False False False) -- Bad Visa card (prefix fails)
    print (creditCardTest 4556641618422 False False False) -- Bad Visa card (luhn fails)
    print (creditCardTest 5580907293574128 False True False) -- Master card
    print (creditCardTest 558090729357412 False False False) -- Master card (length fails)
    print (creditCardTest 1580907293574128 False False False) -- Bad Master card (prefix fails)
    print (creditCardTest 5580907293574127 False False False) -- Bad Master card (luhn fails)
    print (creditCardTest 373993851220785 False False True) -- American Express card
    print (creditCardTest 37399385122078 False False False ) -- Bad American Express card (length fails )
    print (creditCardTest 873993851220785 False False False) -- American Express card (prefix fails)
    print (creditCardTest 373993851220784 False False False) -- American Express card (luhn fails)