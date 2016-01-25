{- Towers of Hanoi
 -
 - https://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf
 -
 - author: Andrei Alexandru
 -
 - -}

type Peg = String
type Move = (Peg, Peg)

{- Exercise 5 -}

-- hanoi : Recieves the number of disks and the 3 peg names and returns a list
--         of peg pairs representing the moves
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n p1 p2 p3 = hanoi (n-1) p1 p3 p2 ++ [(p1, p2)] ++ hanoi (n-1) p3 p2 p1


{- Exercise 6 !!NOT FINISHED!! -}
-- hanoi4 : Recieves the numer of disks and the 4 peg names and returns a list
--        of peg pairs representing the moves
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 n p1 p2 p3 p4 = hanoi (n-2) p1 p2 p3 ++
                       hanoi (n-2) p1 p2 p4 ++
                       [(p1, p2)] ++
                       hanoi (n-2) p4 p2 p1 ++
                       hanoi (n-2) p3 p2 p1

