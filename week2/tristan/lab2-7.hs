-- This took ~20 minutes

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

iban :: String -> Bool
iban s = do
    let rotated = rotate4 s
    let digits = lettersToDigit rotated
    let number = read digits
    (mod number 97) == 1

rotate4 :: String -> String
rotate4 (s1:s2:s3:s4:s) = s ++ [s1, s2, s3, s4]

lettersToDigit :: String -> String
lettersToDigit [] = []
lettersToDigit (c:s) = letterToDigit c ++ lettersToDigit s

letterToDigit :: Char -> String
letterToDigit c
    | elem c ['A'..'Z'] = show ((ord c) - 55)
    | elem c [' '] = []
    | otherwise = [c]

main = do
    print (iban "AL47 2121 1009 0000 0002 3569 8741")