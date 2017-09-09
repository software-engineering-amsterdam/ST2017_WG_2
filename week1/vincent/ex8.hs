-- ID: 11408227
-- Name: Vincent Jong
-- Time: ~4 hrs

import Data.List

data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq, Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Matthew x = x /= Carl && x /= Matthew
accuses Peter x = x == Matthew || x == Jack
accuses Jack x = not (accuses Matthew x) && not (accuses Peter x)
accuses Arnold x = accuses Matthew x /= accuses Peter x
accuses Carl x = not (accuses Arnold x)
--accuses Matthew x = False
--accuses Matthew Carl = False
--accuses Peter Matthew = True
--accuses Peter Jack = True
--accuses Jack Matthew = True
--accuses Jack Peter = True
--accuses Arnold Matthew = True
--accuses Arnold Peter = False
--accuses Arnold Matthew = False
--accuses Arnold Peter = True
--accuses Carl Arnold = True
--accuses x y = False

accusers :: Boy -> [Boy]
accusers x = [ y | y <- boys, accuses y x ]

-- Get the boy that is accused by 3 people (we assume from the teacher that 3 people are telling the truth)
guilty :: [Boy]
guilty = [ x | x <- boys, length (accusers x) == 3 ]

honest :: [Boy]
honest = [ x | x <- boys, accuses x $ head guilty]