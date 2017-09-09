-- This took ~3 hours

import Data.List

data Boy = Matthew | Peter | Jack | Arnold | Carl 
    deriving (Eq, Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Matthew boy = not (boy == Carl) && not (boy == Matthew)
accuses Peter boy = boy == Matthew || boy == Jack
accuses Jack boy = not (accuses Matthew boy) && not (accuses Peter boy)
accuses Arnold boy = accuses Matthew boy /= accuses Peter boy
accuses Carl boy = not (accuses Arnold boy)

accusers :: Boy -> [Boy]
accusers boy = filter (\x -> accuses x boy) boys

guilty, honest :: [Boy]
guilty = filter (\x -> (length (accusers x)) == 3) boys
honest = accusers (head guilty)

main = do
    print (guilty)
    print (honest)