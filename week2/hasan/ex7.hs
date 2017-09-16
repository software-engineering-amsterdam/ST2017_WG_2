--Q:Note It is not enough to test only with correct examples. You should invent a way to test with incorrect examples also.
--Can you automate the test process?
--A:Yes, you can change one digit of every correct IBAN because it would return False

module Lab2Ex7 where

import Data.List
import Data.Char

alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

iban :: String -> Bool
iban n = ibanStep3 (ibanStep2 (ibanStep1 (ibanStep0 n))) == 1

ibanStep0, ibanStep1, ibanStep2 :: [Char] -> [Char] 
ibanStep0 n = filter (\x -> not (isSpace x)) n
ibanStep1 n = (drop 4 n) ++ (take 4 n)
ibanStep2 n = concat (map charConvert n)

ibanStep3 :: [Char] -> Integer
ibanStep3 n = mod x 97 where x = read n :: Integer

charConvert :: Char -> [Char]
charConvert n = case elemIndex n alphabet of
                    Nothing -> [n]
                    Just x -> show (x+10)

main = do
    print (iban "NL39 RABO 0300 0652 64")