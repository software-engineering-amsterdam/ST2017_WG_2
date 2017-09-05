-- Started at 13:15, stopped 15:30

module Lab1 where
import Data.List
import Test.QuickCheck

data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-- Determined on paper by writing down who accuses who (directly or indirectly)
accuses :: Boy -> Boy -> Bool
accuses Matthew Peter = True
accuses Matthew Jack = True
accuses Matthew Arnold = True
accuses Peter Matthew = True
accuses Peter Jack = True
accuses Jack Carl = True
accuses Jack Arnold = True
accuses Arnold Peter = True
accuses Arnold Matthew = True
accuses Carl Jack = True
accuses a b = False

accusers :: Boy -> [Boy]
accusers t = [f | f <- boys, accuses f t]

-- jack
guilty :: [Boy]
guilty = [b | b <- boys, length (accusers b) == 3]

-- matthew, peter, carl
honest :: [Boy]
honest = accusers (head guilty)