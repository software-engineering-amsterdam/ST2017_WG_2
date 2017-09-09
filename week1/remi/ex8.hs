-- Started at 13:15, stopped 15:30
-- Evening: started at 19:45, finished at 20:15

module Lab1 where
import Data.List
import Test.QuickCheck

data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-- First I wrote out all cases by determing who (directly or indirectly)
-- accuses who. But that way I was solving part of the programming problem
-- on paper... Then realized we can just write out the boys' statements as
-- boolean expressions. (Which is also a lot easier than working everything
-- out on paper...)
-- For Arnold, /= is used which functions as an exclusive OR, as
-- Matthew or Peter is speaking the truth, but not both.
accuses :: Boy -> Boy -> Bool
accuses Matthew b = not (b == Carl) && not (b == Matthew)
accuses Peter b = b == Matthew || b == Jack
accuses Jack b = not (accuses Matthew b) && not (accuses Peter b)
accuses Arnold b = accuses Matthew b /= accuses Peter b
accuses Carl b = not (accuses Arnold b)

accusers :: Boy -> [Boy]
accusers t = [f | f <- boys, accuses f t]

-- Three people always speak the truth, so find who's accused by exactly three people.
-- The other two always lie, so it's impossible that they accussed the guilty one.
guilty :: [Boy]
guilty = [b | b <- boys, length (accusers b) == 3]

honest :: [Boy]
honest = accusers (head guilty)