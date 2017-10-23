-- This took ~10 minutes

module Ex3 (composites) where

import Lecture6

composites :: [Integer]
composites = filter (not . prime) [2..]

main = print composites
