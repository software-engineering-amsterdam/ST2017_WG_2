-- Took about 2 hours

module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- List of length by country code thanks to https://stackoverflow.com/a/26926469
countryLengths :: [(String, Integer)]
countryLengths = [("AL",28),("AD",24),("AT",20),("AZ",28),("BH",22),("BE",16),("BA",20),
                  ("BR",29),("BG",22),("CR",21),("HR",21),("CY",28),("CZ",24),("DK",18),
                  ("DO",28),("EE",20),("FO",18),("FI",18),("FR",27),("GE",22),("DE",22),
                  ("GI",23),("GR",27),("GL",18),("GT",28),("HU",28),("IS",26),("IE",22),
                  ("IL",23),("IT",27),("JO",30),("KZ",20),("KW",30),("LV",21),("LB",28),
                  ("LI",21),("LT",20),("LU",20),("MK",19),("MT",31),("MR",27),("MU",30),
                  ("MC",27),("MD",24),("ME",22),("NL",18),("NO",15),("PK",24),("PS",29),
                  ("PL",28),("PT",25),("QA",29),("RO",24),("SM",27),("SA",24),("RS",22),
                  ("SK",24),("SI",19),("ES",24),("SE",24),("CH",21),("TN",24),("TR",26),
                  ("AE",23),("GB",22),("VG",24)]

-- Get length from above list by country
countryLength :: String -> Integer
countryLength x = snd (head (filter (\y -> fst y == x) countryLengths))

removeSpaces :: String -> String
removeSpaces x = filter (\y -> y /= ' ') x

rearrangeIban :: String -> String
rearrangeIban x = drop 4 x ++ take 4 x

letterToInt :: Char -> String
letterToInt x
    | isUpper x = show (ord x - 55)
    | otherwise = [x]

-- Map returns array, so concat to put it together again
stringToInt :: String -> Integer
stringToInt x = read (concat (map letterToInt x)) :: Integer

-- TODO: LENGTH
iban :: String -> Bool
iban x = genericLength i == l && c `mod` 97 == 1
    where i = removeSpaces x
          r = rearrangeIban i
          c = stringToInt r
          l = countryLength (take 2 i)

main = iban "GB82 WEST 1234 5698 7654 32"