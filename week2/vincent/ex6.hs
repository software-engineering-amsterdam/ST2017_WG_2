-- ID: 11408227
-- Name: Vincent Jong
-- Time: 21:20 - 21:50 (Programming), (Answering)

-- Specification:
-- Letters that are moved 13 spots ahead should be the same letter when moved another 13 spots ahead

import Data.List
import Data.Char
import Test.QuickCheck

getNewChar :: Char -> Char
getNewChar x
    | elem x ['a'..'z'] = toEnum $ (fromEnum x - fromEnum 'a' + 13) `mod` 26 + fromEnum 'a'
    | elem x ['A'..'Z'] = toEnum $ (fromEnum x - fromEnum 'A' + 13) `mod` 26 + fromEnum 'A'
    | otherwise = x
    --let lowercase = isLower x
    --let newIndex = (fromEnum x) + 13
    --let newIndexFixed = if lowercase then if newIndex > 122 then (newIndex - 26) else newIndex else if newIndex > 92 then (newIndex - 26) else newIndex 
    --toEnum newIndexFixed :: Char

rot13 :: String -> String
rot13 [] = []
rot13 (c:cs) = [getNewChar c] ++ rot13 cs

inverseIsSame :: [Char] -> Bool
inverseIsSame cs = rot13 (rot13 cs) == cs

nonLettersIsSame :: Char -> Bool
nonLettersIsSame c = if not (elem c ['A'..'z']) then getNewChar c == c else True

main = do
    quickCheck nonLettersIsSame
    quickCheck inverseIsSame