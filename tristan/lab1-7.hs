-- This took ~2 hours

import Data.List
import Test.QuickCheck

-- For cases where luhn numbers are needed the following generator is used
generateLuhnInteger :: Gen Integer
generateLuhnInteger = abs `fmap` (arbitrary :: Gen Integer) `suchThat` (luhn)

luhn :: Integer -> Bool
luhn x = do
    let reduced = reduceLarge (doubleEverySecond (reverse (digits x)))
    let summed = sum reduced
    rem (summed) 10 == 0

doubleEverySecond :: [Integer] -> [Integer]
doubleEverySecond [] = []
doubleEverySecond (x:y:xs) = x : y*2 : doubleEverySecond xs
doubleEverySecond (x:xs) = x : doubleEverySecond xs

-- Subtracts 9 from integers larger than 9
reduceLarge :: [Integer] -> [Integer]
reduceLarge [] = []
reduceLarge (x:xs)
    | x > 9 = x - 9 : reduceLarge xs
    | otherwise = x : reduceLarge xs

digits :: Integer -> [Integer]
digits = map (read . return) . show

isAnyPrefix :: [Integer] -> Integer -> Bool
isAnyPrefix x d = do
    let totalDigits = digits d
    any (\y -> digitsStartWith (digits y) totalDigits) x

digitsStartWith :: [Integer] -> [Integer] -> Bool
digitsStartWith x d = isPrefixOf x d

isAnyLength :: [Integer] -> Integer -> Bool
isAnyLength r x = any (\y -> y == x) r

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress x = isAnyLength [15] (genericLength (digits x)) && luhn x && isAnyPrefix [34, 37] x
isMaster x = isAnyLength [16] (genericLength (digits x)) && luhn x && isAnyPrefix ([51..55]++[2221..2720]) x
isVisa x = isAnyLength [13, 16, 19] (genericLength (digits x)) && luhn x && isAnyPrefix [4] x

prop_card_test :: Integer -> Bool
prop_card_test x 
    | luhn x = do
        let ia = isAmericanExpress x
        let im = isMaster x
        let iv = isVisa x
        (not (ia) && not (im) && not (iv)) || (ia /= im && im /= iv)
    | otherwise = not (isAmericanExpress x) && not (isMaster x) && not (isVisa x) 

main = quickCheck (forAll generateLuhnInteger prop_card_test)

