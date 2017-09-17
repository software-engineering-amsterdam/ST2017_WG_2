-- Started at 15:10, finished triangle function at 15:25
-- Finished testing/explaining my method of testing at 16:00

-- NoTriangle, Equilateral and Isosceles can be tested using quickCheck.
-- Their preconditions are relatively simple: NoTriangle needs at least one of three
-- of the random integers to be <= 0. Equilateral only needs one positive integer,
-- as all three legs are equal. Isosceles only needs two different positive integers,
-- as two of three legs are equal.

-- It is hard to test for Rectangular using a random test generator.
-- It is impossible to get a significant
-- amount of random (a,b,c) for which precondition a^2+b^2=c^2
-- or a^2+c^2=b^2 or b^2+c^2=a^2 holds.
-- Therefore, it makes more sense to check the function with a significant amount of examples
-- known to be rectangular.

-- It is possible to test for Other, as it needs three different positive numbers,
-- for which a^2+b^2=c^2, a^2+c^2=b^2 and b^2+c^2=a^2 do not hold. QuickCheck is sometimes
-- able to find 100 (a,b,c) for this, however, it often gives up after ~70-90 passed tests.

module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

isTriangle, isEquilateral, isIsosceles, isRectangular :: Int -> Int -> Int -> Bool

isTriangle a b c = a > 0 && b > 0 && c > 0

isEquilateral a b c = a == b && b == c

-- For this function to be correct for use outside of the triangle function,
-- we should check if it is not equilateral
isIsosceles a b c = (a == b || a == c || b == c) && not (isEquilateral a b c)

isRectangular a b c = a2 + b2 == c2 || a2 + c2 == b2 || b2 + c2 == a2
    where a2 = a^2
          b2 = b^2
          c2 = c^2

triangle :: Int -> Int -> Int -> Shape
triangle a b c
    | not (isTriangle a b c) = NoTriangle
    | isEquilateral a b c = Equilateral
    | isIsosceles a b c = Isosceles
    | isRectangular a b c = Rectangular
    | otherwise = Other

-- Tests
noTriangleTest = quickCheck (\a b c -> a <= 0 || b <= 0 || c <= 0 ==> triangle a b c == NoTriangle)
equilateralTest = quickCheck (\a -> a > 0 ==> triangle a a a == Equilateral)

-- For completeness, we could also test a b a and b a a as arguments, so we test if it works in every order
isoscelesTest = quickCheck (\a b -> a > 0 && b > 0 && a /= b ==> triangle a a b == Isosceles)

-- Use isRectangular to check if a^2+b^2=c^2, a^2+c^2=b^2 and b^2+c^2=a^2 do not hold
otherTest = quickCheck (\a b c -> a > 0 && b > 0 && c > 0 && a /= b && b /= c && a /= c && not (isRectangular a b c) ==> triangle a b c == Other)

-- Of course, we would need more examples to thoroughly test for rectangular triangles
rectangularTest = (triangle 3 4 5) == Rectangular

main = do
    equilateralTest
    noTriangleTest
    isoscelesTest
    otherTest