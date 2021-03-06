-- The triangle function, with all sub functions (isNotTriangle, isEquilateral, isIsosceles, isRectangular)
-- are tested by quickCheck and checked known triangles.
-- 
-- QuickCheck: Generate random triangles with sides a b c.
--             When one of the sides is negative, an NoTriangle result should be generated
--             These triangles are skipped.
--             In all other cases, we check if isEquilateral, isIsosceles, isRectangular produce just one truthy value.
--             When more than one of these functions generates a True for (a, b, c) the test fails.
-- 
-- With quickCheck, we test if our specification maps all combinations of a b c to one of our defined triangles.
--
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

-- Lastly, we provided a few known triangles and test whether these generate the expected output.
-- 
-- This took 1.5 hours

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other 
    deriving (Eq, Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | isNotTriangle a b c = NoTriangle
    | isEquilateral a b c = Equilateral
    | isIsosceles a b c = Isosceles
    | isRectangular a b c = Rectangular
    | otherwise = Other

isNotTriangle, isEquilateral, isIsosceles, isRectangular :: Integer -> Integer -> Integer -> Bool

isNotTriangle a b c = a <= 0 || b <= 0 || c <= 0

isEquilateral a b c = a == b && b == c

isIsosceles a b c = (a == b || a == c || b == c) && not (isEquilateral a b c)

isRectangular a b c = do
    let [c2, b2, a2] = sortBy (flip compare) [a, b, c]
    a^2 + b^2 == c^2

-- Tests
noTriangleTest = quickCheck (\a b c -> a <= 0 || b <= 0 || c <= 0 ==> triangle a b c == NoTriangle)
equilateralTest = quickCheck (\a -> a > 0 ==> triangle a a a == Equilateral)

-- For completeness, we could also test a b a and b a a as arguments, so we test if it works in every order
isoscelesTest = quickCheck (\a b -> a > 0 && b > 0 && a /= b ==> triangle a a b == Isosceles)

-- Use isRectangular to check if a^2+b^2=c^2, a^2+c^2=b^2 and b^2+c^2=a^2 do not hold
otherTest = quickCheck (\a b c -> a > 0 && b > 0 && c > 0 && a /= b && b /= c && a /= c && not (isRectangular a b c) ==> triangle a b c == Other)

-- Of course, we would need more examples to thoroughly test for rectangular triangles
rectangularTest = (triangle 3 4 5) == Rectangular

prop_shape_unique :: Integer -> Integer -> Integer -> Bool
prop_shape_unique a b c
    | isNotTriangle a b c = True
    | matches > 1 = False
    | otherwise = True
    where x = [isEquilateral a b c, isIsosceles a b c, isRectangular a b c]
          matches = length (filter (==True) x)

main = do
    quickCheck prop_shape_unique
    equilateralTest
    noTriangleTest
    isoscelesTest
    otherTest

    putStrLn "\nChecking various known triangles..."
    
    putStrLn "\nInvalid triangles..."
    print ((triangle 0 0 0) == NoTriangle)
    print ((triangle 1 0 1) == NoTriangle)
    print ((triangle (-1) 1 1) == NoTriangle)

    putStrLn "\nEquilateral triangles..."
    print ((triangle 1 1 1) == Equilateral)
    print ((triangle 2 2 2) == Equilateral)
    print ((triangle 1000 1000 1000) == Equilateral)

    putStrLn "\nIsosceles triangles..."
    print ((triangle 1 1 2) == Isosceles)
    print ((triangle 3 2 2) == Isosceles)
    print ((triangle 1000 1001 1000) == Isosceles)

    putStrLn "\nRectangular triangles..."
    print ((triangle 3 4 5) == Rectangular)
    print ((triangle 5 12 13) == Rectangular)
    print ((triangle 21 220 221) == Rectangular)

    putStrLn "\nOther triangles..."
    print ((triangle 3 823 5) == Other)
    print ((triangle 918273 12 1337) == Other)
    print ((triangle 162 230 9) == Other)