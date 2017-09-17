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
    print (iban "AL47 2121 1009 0000 0002 3569 8741" == True)
    print (iban "AD12 0001 2030 2003 5910 0100" == True)
    print (iban "AT61 1904 3002 3457 3201" == True)
    print (iban "AZ21 NABZ 0000 0000 1370 1000 1944" == True)
    print (iban "BH67 BMAG 0000 1299 1234 56" == True)
    print (iban "BE62 5100 0754 7061" == True)
    print (iban "BA39 1290 0794 0102 8494" == True)
    print (iban "BG80 BNBG 9661 1020 3456 78" == True)
    print (iban "HR12 1001 0051 8630 0016 0" == True)
    print (iban "CY17 0020 0128 0000 0012 0052 7600" == True)
    print (iban "CZ65 0800 0000 1920 0014 5399" == True)
    print (iban "DK50 0040 0440 1162 43" == True)
    print (iban "EE38 2200 2210 2014 5685" == True)
    print (iban "FO97 5432 0388 8999 44" == True)
    print (iban "FI21 1234 5600 0007 85" == True)

    print (iban "NL66 FAKE 0123 4567 89" == True)
    print (iban "NL67 FAKE 0123 4567 89" == False)
    print (iban "NL6 FAKE 0123 4567 89" == False)
    print (iban "AL66 FAKE 0123 4567 89" == False)
    print (iban "FAKE 0123 4567 89" == False)
    print (iban "FAKE 0123 4567 89 NL66" == False)
