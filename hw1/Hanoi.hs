type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 src dst _ = [(src, dst)]
hanoi n src dst tmp = hanoi (n-1) src tmp dst ++ [(src, dst)] ++ hanoi (n-1) tmp dst src
