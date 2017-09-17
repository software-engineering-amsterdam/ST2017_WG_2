-- Took about 45 minutes

-- Specification:
-- Uppercase letters are changed into uppercase letters 13 places ahead
-- Lowercase letters are changed into lowercase letters 13 places ahead
-- Other characters are not changed

-- Testing:
-- As we shift 13 letters and there are 26 in the alphabet,
-- applying the function twice should result into the original string.
-- Therefore, we can test the following property:
-- rot13 (rot13 x) = x
-- We also tested that non-letter characters are not changed.

module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

getNewChar :: Char -> Char
getNewChar x
    | elem x ['a'..'z'] = toEnum $ (fromEnum x - fromEnum 'a' + 13) `mod` 26 + fromEnum 'a'
    | elem x ['A'..'Z'] = toEnum $ (fromEnum x - fromEnum 'A' + 13) `mod` 26 + fromEnum 'A'
    | otherwise = x

rot13 :: String -> String
rot13 x = map getNewChar x

-- Properties
propOwnInverse :: String -> Bool
propOwnInverse x = x == rot13 (rot13 x)

propNonLettersIsSame :: String -> Bool
propNonLettersIsSame c = o == os
    where cs = rot13 c
          o = filter (\x -> not (elem x ['A'..'z'])) c
          os = filter (\x -> not (elem x ['A'..'z'])) cs

main = do
    quickCheck propOwnInverse
    quickCheck propNonLettersIsSame
