-- About 2 hours

module Lab1 where
import Data.List
import Test.QuickCheck

data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-- Boolean formulas
accuses :: Boy -> Boy -> Bool
accuses Matthew x = not (elem x [Carl, Matthew])
accuses Peter x = elem x [Matthew, Jack]
accuses Jack x = not(accuses Matthew x) && not (accuses Peter x)
accuses Arnold x = (accuses Matthew x) /= (accuses Peter x)
accuses Carl x = not(accuses Arnold x)

accusers :: Boy -> [Boy]
accusers t = [f | f <- boys, accuses f t]

-- Three people always speak the truth, so find who's accused by exactly three people.
-- The other two always lie, so it's impossible that they accussed the guilty one.
guilty :: [Boy]
guilty = [b | b <- boys, length (accusers b) == 3]

honest :: [Boy]
honest = accusers (head guilty)

main = do
    print "Guilty"
    print guilty
    print "Honest"
    print honest