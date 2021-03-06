-- Took about 2.5 hours

-- Tested using a list of IBAN numbers known to be correct and incorrect.
-- Examples of wrong numbers created by changing letters or length of correct numbers to
-- incorrect format. Also by changing one of the numbers, which causes
-- the checksum to be wrong.

-- Using the Hoare test function, these lists of IBAN numbers can easily be tested.

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
countryLength x
    | length country > 0 = snd (head (country))
    | otherwise = 0
        where country = filter (\y -> fst y == x) countryLengths

removeSpaces :: String -> String
removeSpaces x = filter (\y -> not (isSpace y)) x

rearrangeIban :: String -> String
rearrangeIban x = drop 4 x ++ take 4 x

letterToInt :: Char -> String
letterToInt x
    | isUpper x = show (ord x - 55)
    | otherwise = [x]

-- Map returns array, so concat to put it together again
stringToInt :: String -> Integer
stringToInt x = read (concat (map letterToInt x)) :: Integer

iban :: String -> Bool
iban x = genericLength i == l && c `mod` 97 == 1
    where i = removeSpaces x
          r = rearrangeIban i
          c = stringToInt r
          l = countryLength (take 2 i)

correctIban :: [String]
correctIban = ["AL47 2121 1009 0000 0002 3569 8741",
            "AD12 0001 2030 2003 5910 0100",
            "AT61 1904 3002 3457 3201",
            "AZ21 NABZ 0000 0000 1370 1000 1944",
            "BH67 BMAG 0000 1299 1234 56",
            "BE62 5100 0754 7061",
            "BA39 1290 0794 0102 8494",
            "BG80 BNBG 9661 1020 3456 78",
            "HR12 1001 0051 8630 0016 0",
            "CY17 0020 0128 0000 0012 0052 7600",
            "CZ65 0800 0000 1920 0014 5399",
            "DK50 0040 0440 1162 43",
            "EE38 2200 2210 2014 5685",
            "FO97 5432 0388 8999 44",
            "FI21 1234 5600 0007 85",
            "FR14 2004 1010 0505 0001 3M02 606",
            "GE29 NB00 0000 0101 9049 17",
            "DE89 3704 0044 0532 0130 00",
            "GI75 NWBK 0000 0000 7099 453",
            "GR16 0110 1250 0000 0001 2300 695",
            "GL56 0444 9876 5432 10",
            "HU42 1177 3016 1111 1018 0000 0000",
            "IS14 0159 2600 7654 5510 7303 39",
            "IE29 AIBK 9311 5212 3456 78",
            "IL62 0108 0000 0009 9999 999",
            "IT40 S054 2811 1010 0000 0123 456",
            "JO94 CBJO 0010 0000 0000 0131 0003 02",
            "KW81 CBKU 0000 0000 0000 1234 5601 01",
            "LV80 BANK 0000 4351 9500 1",
            "LB62 0999 0000 0001 0019 0122 9114",
            "LI21 0881 0000 2324 013A A",
            "LT12 1000 0111 0100 1000",
            "LU28 0019 4006 4475 0000",
            "MK072 5012 0000 0589 84",
            "MT84 MALT 0110 0001 2345 MTLC AST0 01S",
            "MU17 BOMM 0101 1010 3030 0200 000M UR",
            "MD24 AG00 0225 1000 1310 4168",
            "MC93 2005 2222 1001 1223 3M44 555",
            "ME25 5050 0001 2345 6789 51",
            "NL39 RABO 0300 0652 64",
            "NO93 8601 1117 947",
            "PK36 SCBL 0000 0011 2345 6702",
            "PL60 1020 1026 0000 0422 7020 1111",
            "PT50 0002 0123 1234 5678 9015 4",
            "QA58 DOHB 0000 1234 5678 90AB CDEF G",
            "RO49 AAAA 1B31 0075 9384 0000",
            "SM86 U032 2509 8000 0000 0270 100",
            "SA03 8000 0000 6080 1016 7519",
            "RS35 2600 0560 1001 6113 79",
            "SK31 1200 0000 1987 4263 7541",
            "SI56 1910 0000 0123 438",
            "ES80 2310 0001 1800 0001 2345",
            "SE35 5000 0000 0549 1000 0003",
            "CH93 0076 2011 6238 5295 7",
            "TN59 1000 6035 1835 9847 8831",
            "TR33 0006 1005 1978 6457 8413 26",
            "AE07 0331 2345 6789 0123 456"]

wrongIban :: [String]
wrongIban = ["ALB7 2121 1009 0000 0002 3569 8742",
            "AD12 0001 2030 2003 5910 0101",
            "AT81 1904 3002 3457 3201",
            "AZ21 N9BZ 0000 0000 1370 1000 1944",
            "BH67 BMAG 0100 1299 1234 56",
            "BE62 5100 0754 7062",
            "BA39 1290 0794 0102 8495",
            "BG80 BNBG 9661 1020 3456 79",
            "HR12 1001 0051 8630 0016 1",
            "NL17 0020 0128 0000 0012 0052 7600",
            "SU56 1910 0000 0123 438",
            "ES80 2311 0001 1800 0001 2345",
            "SE35 5000 0001 0549 1000 0003",
            "CH93 0076 2011 6239 5295 7",
            "TN59 1000 6035 1835 9848 8831",
            "TR33 0006 1005 1978 6457 8414 26"]

-- Hoare test function
hoareTest :: (a -> Bool) -> (a -> Bool) -> (Bool -> Bool) -> [a] -> Bool
hoareTest precondition f postcondition =
    all (\x -> precondition x --> postcondition (f x))

main = do
    -- Test with correct IBANs
    print (hoareTest (\_ -> True) iban (\x -> x) correctIban)

    -- Test with incorrect IBANs
    print (hoareTest (\_ -> True) iban (\x -> not x) wrongIban)