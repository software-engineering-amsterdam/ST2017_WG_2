-- ID: 11408227
-- Name: Vincent Jong
-- Time: ~4 hrs

import Data.List
import Data.Char (digitToInt) -- Assignment 7
import Data.Sequence hiding (length, take, filter, reverse, drop, zipWith) -- Assignment 7
import Data.Foldable (toList) -- Assignment 7

-- Helper functions

-- Convert Integer to list of Integers, from https://stackoverflow.com/questions/24346667/haskell-converting-integer-into-list-of-digits
toDigitsList :: Integer -> [Integer]
toDigitsList = map (fromIntegral . digitToInt) . show

-- Convert list of integers into one integer e.g. [1,2,3] to 123
-- From: https://stackoverflow.com/questions/1918486/convert-list-of-integers-into-one-int-like-concat-in-haskell
toSingleInteger = foldl addDigit 0
    where addDigit num d = 10 * num + d


luhn_doubleIntegers :: [Integer] -> Int -> [Integer]
luhn_doubleIntegers xs y = 
    if y >= 0
        then do 
            let z = if (xs !! y) * 2 > 9 then (xs !! y) * 2 - 9 else (xs !! y) * 2
            let ys = toList (update y z (fromList xs))
            luhn_doubleIntegers (ys) (y-2)
        else
            xs

luhn :: Integer -> Bool
luhn x = do 
    let list = toDigitsList x
    let indexToDouble = length list - 2
    let doubledList = luhn_doubleIntegers list indexToDouble
    if (sum doubledList) `mod` 10 == 0 then True else False 
            
isAmericanExpress :: Integer -> Bool
isAmericanExpress x = do
    let list = toDigitsList x
    let iin = toSingleInteger $ take 2 list
    if length list == 15 && (iin == 34 || iin == 37) && luhn x then True else False

isMaster :: Integer -> Bool
isMaster x = do
    let list = toDigitsList x
    let iin_1 = toSingleInteger $ take 2 list
    let iin_2 = toSingleInteger $ take 4 list
    if length list == 16 && ((iin_1 >= 51 && iin_1 <= 55) || (iin_2 >= 2221 && iin_2 <= 2720)) && luhn x then True else False

isVisa :: Integer -> Bool
isVisa x = do
    let list = toDigitsList x
    let validLength = length list == 13 || length list == 16 || length list == 19
    let iin = head list
    if validLength && (iin == 4) && luhn x then True else False

-- Lists composed from Paypal websites: 
-- https://www.paypalobjects.com/en_US/vhelp/paypalmanager_help/credit_card_numbers.htm
-- http://www.getcreditcardnumbers.com/
validAmericanExpressList = [371449635398431, 378282246310005, 378734493671000]
invalidAmericanExpressList = [371449635398434, 371449635398435, 371449635398438]
validMasterCardList = [5555555555554444, 5105105105105100, 5372760688551926]
invalidMasterCardList = [5372760688551922, 5372760688551924, 5372760688551929]
validVisaCardList = [4111111111111111, 4012888888881881, 4222222222222]
invalidVisaCardList = [4012888888881884, 4012888888881887, 4012888888881883]

-- Check a list if it simultaneously is valid for a certain type of card and fails for the other types
testList :: [Integer] -> Bool -> Bool -> Bool -> Bool
testList [] isAmExpr isMas isVis = True
testList (x:xs) isAmExpr isMas isVis = (isAmericanExpress x) == isAmExpr && (isMaster x) == isMas && (isVisa x) == isVis && testList xs isAmExpr isMas isVis

main = 
    do
        putStrLn "Checking American Express cards: [371449635398431, 378282246310005, 378734493671000]\n"
        if testList validAmericanExpressList True False False then putStrLn "Check success: Passed" else putStrLn "Check success: Failed"
        if testList invalidAmericanExpressList False False False then putStrLn "Check fail: Passed\n" else putStrLn "Check fail: Failed\n"
        putStrLn "Checking Master cards: [5555555555554444, 5105105105105100, 5372760688551926]\n"
        if testList validMasterCardList False True False then putStrLn "Check success: Passed" else putStrLn "Check success: Failed"
        if testList invalidMasterCardList False False False then putStrLn "Check fail: Passed\n" else putStrLn "Check fail: Failed\n"
        putStrLn "Checking Visa cards: [4111111111111111, 4012888888881881, 4222222222222]\n"
        if testList validVisaCardList False False True then putStrLn "Check success: Passed" else putStrLn "Check: Failed"
        if testList invalidVisaCardList False False False then putStrLn "Check fail: Passed\n" else putStrLn "Check fail: Failed\n"