-- This took ~45 minutes
--
-- Tests were performed using Criterion (http://hackage.haskell.org/package/criterion)
-- Results:
--
-- benchmarking expM/2^168277 mod 673109
-- time                 418.4 μs   (414.7 μs .. 423.9 μs)
--                      0.998 R²   (0.995 R² .. 1.000 R²)
-- mean                 256.6 μs   (247.1 μs .. 266.0 μs)
-- std dev              13.38 μs   (8.323 μs .. 21.49 μs)
-- variance introduced by outliers: 40% (moderately inflated)
--
-- benchmarking expM/7838761^3 mod 9861364
-- time                 733.3 ns   (728.4 ns .. 738.7 ns)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 426.9 ns   (399.9 ns .. 449.1 ns)
-- std dev              27.15 ns   (11.23 ns .. 34.25 ns)
-- variance introduced by outliers: 74% (severely inflated)
--
-- benchmarking expM/98328^168277 mod 17982571
-- time                 13.98 ms   (13.70 ms .. 14.30 ms)
--                      0.998 R²   (0.997 R² .. 0.999 R²)
-- mean                 11.70 ms   (11.22 ms .. 12.15 ms)
-- std dev              1.064 ms   (784.1 μs .. 1.338 ms)
-- variance introduced by outliers: 42% (moderately inflated)
--
-- benchmarking exMEfficient/2^168277 mod 673109
-- time                 19.96 μs   (19.89 μs .. 20.05 μs)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 11.35 μs   (10.88 μs .. 11.85 μs)
-- std dev              602.6 ns   (287.1 ns .. 818.2 ns)
-- variance introduced by outliers: 57% (severely inflated)
--
-- benchmarking exMEfficient/7838761^3 mod 9861364
-- time                 2.893 μs   (2.882 μs .. 2.902 μs)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 1.687 μs   (1.601 μs .. 1.742 μs)
-- std dev              84.25 ns   (34.92 ns .. 111.0 ns)
-- variance introduced by outliers: 60% (severely inflated)
--
-- benchmarking exMEfficient/98328^168277 mod 17982571
-- time                 20.26 μs   (20.07 μs .. 20.53 μs)
--                      0.998 R²   (0.997 R² .. 1.000 R²)
-- mean                 11.46 μs   (10.91 μs .. 11.88 μs)
-- std dev              566.4 ns   (274.0 ns .. 778.7 ns)
-- variance introduced by outliers: 53% (severely inflated)
--
-- It shows that the Right-to-left binary method is extremely fast in comaprion to the naive approach
-- when applying to huge exponents with larger base. The case where the exponent is small but the base is large
-- gives the worst performance ∏for this algorithm, since it is not optimised for small exponents.


import Ex1
import Lecture6
import Criterion.Main

benchExpM :: (Integer, Integer, Integer) -> Integer
benchExpM (b, e, m) = expM b e m

benchExMEfficient :: (Integer, Integer, Integer) -> Integer
benchExMEfficient (b, e, m) = exMEfficient b e m

main = defaultMain [
    bgroup "expM" [ bench "2^168277 mod 673109" $ nf benchExpM (2, 168277, 673109)
                  , bench "7838761^3 mod 9861364" $ nf benchExpM (7838761, 3, 9861364)
                  , bench "98328^168277 mod 17982571" $ nf benchExpM (98328, 168277, 17982571)],

    bgroup "exMEfficient" [ bench "2^168277 mod 673109" $ nf benchExMEfficient (2, 168277, 673109)
                          , bench "7838761^3 mod 9861364" $ nf benchExMEfficient (7838761, 3, 9861364)
                          , bench "98328^168277 mod 17982571" $ nf benchExMEfficient (98328, 168277, 17982571)]
    ]
