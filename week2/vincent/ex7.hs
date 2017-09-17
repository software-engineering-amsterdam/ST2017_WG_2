-- ID: 11408227
-- Name: Vincent Jong
-- Time: 21:50 - 22:50, 21:00 - 22:15 (Programming), (Answering)

-- Tested using correct and incorrect numbers from https://ssl.ibanrechner.de/sample_accounts.html?&L=0
-- Testing for incorrect numbers could be automated by modifying correct numbers in some way.

import Data.List
import Data.Char
import Data.Foldable

testNr = "GB29NWBK60161331926819" -- GB29 NWBK 6016 1331 9268 19
testNr2 = "GB29 NWBK 6016 1331 9268 19"

-- lengths from https://www.dnb.no/en/business/transaction-banking/international-payments/example-iban.html
countryIbanLengths :: [(String, Integer)]
countryIbanLengths = [("AD", 24), ("AT", 20), ("BH", 22), ("BE", 16), ("BA", 20), ("BG", 22), 
                      ("HR", 21), ("CY", 28), ("CZ", 24), ("DK", 18), ("EE", 20), ("FO", 18), 
                      ("FI", 18), ("FR", 27), ("GE", 22), ("DE", 22), ("GI", 23), ("GB", 22), 
                      ("HU", 28), ("IS", 26), ("IE", 22), ("IT", 27), ("JO", 30), ("KZ", 20), 
                      ("KW", 30), ("LV", 21), ("LB", 28), ("LI", 21), ("LT", 20), ("LU", 20), 
                      ("MK", 19), ("MT", 31), ("MD", 24), ("MC", 27), ("ME", 22), ("NL", 18), 
                      ("NO", 15), ("PS", 29), ("PL", 28), ("PT", 25), ("QA", 29), ("RO", 24), 
                      ("SM", 27), ("SA", 24), ("SK", 24), ("SI", 19), ("ES", 24), ("SE", 24), 
                      ("CH", 21), ("TN", 24), ("TR", 26), ("AE", 23), ("AL", 28), ("AZ", 28),
                      ("GR", 27), ("GL", 18), ("IL", 23)]

getCountryIbanLength :: String -> Integer
getCountryIbanLength code = snd (head country)
    where country = filter (\x -> fst x == code) countryIbanLengths

removeSpaces :: String -> String
removeSpaces xs = filter (\x -> x /= ' ') xs

getInteger :: Char -> Integer
getInteger x = if isDigit x then fromIntegral (digitToInt x) else fromIntegral ((fromEnum x) - 55)

toSingleInteger :: [Integer] -> Integer
toSingleInteger = read . concatMap show

iban :: String -> Bool
iban cs = do
    let filtered = removeSpaces cs
    let rearranged = drop 4 filtered ++ take 4 filtered
    let toInteger = map getInteger rearranged
    let singleInteger = toSingleInteger toInteger
    getCountryIbanLength (take 2 cs) == fromIntegral (length filtered) && singleInteger `mod` 97 == 1

-- from https://ssl.ibanrechner.de/sample_accounts.html?&L=0
trueIban = ["AL90208110080000001039531801", "AT022050302101023600", "BE68844010370034",
            "CZ4201000000195505030267", "DK5750510001322617", "EE342200221034126658",
            "FI9814283500171141", "FR7630066100410001057380116", "DE12500105170648489890",
            "HU29117080012054779400000000", "IE92BOFI90001710027952", "IT68D0300203280000400162854",
            "LI1008800000020176306", "LU761111000872960000", "MT98MMEB44093000000009027293051",
            "MC1112739000700011111000H79", "NL18ABNA0484869868", "NO5015032080119",
            "PL37109024020000000610000434", "PT50003506830000000784311", "SM86U0322509800000000270100",
            "SE6412000000012170145230", "CH3908704016075473007", "SK9311110000001057361004",
            "SI56031001001300933", "ES1020903200500041045040", "GB32ESSE40486562136016"]

falseIban = ["AL901490122010010999", "AT021490122010010999", "BE68844010370134",
             "CZ4201000000185505030267", "DK57777777777", "EE34221034126650",
             "FI98142835171040", "FR7630066100410001057380111", "DE1250010517064847930",
             "HU291170801120547794", "IE92BOFI90001711127952", "IT68E0300203280000400162854",
             "LI100880122176406", "LU761111000872960001", "MT98MMEB77777484169867",
             "MC1112739000700011111000L79", "NL18ABNA0484869867", "NO5015032080118",
             "PL37109024020000000610000435", "PT078101120000000784310", "SMZ0322509800000000270100",
             "SE1217145129", "CH870416075473000", "SK10573610041119",
             "SI031001001300930", "ES20903200510041045040", "GB4027152289890"]

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

hoareTest :: (String -> Bool) -> (String -> Bool) -> (Bool -> Bool) -> [String] -> Bool
hoareTest precondition f postcondition =
    all (\x -> precondition x --> postcondition (f x))

main = do
    putStrLn "Checking valid IBAN"
    print (hoareTest (\_ -> True) iban (\x -> x) trueIban)
    putStrLn "Checking invalid IBAN"
    print (hoareTest (\_ -> True) iban (\x -> not x) falseIban)