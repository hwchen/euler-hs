-- euler 18
-- max sum path through triangle


type Row = [Int]
type Triangle = [Row]
type Path = [Int]



row1 = [75]
row2 = [95,64]
row3 = [17,47,82]
row4 = [18,35,87,10]
row5 = [20,04,82,47,65]

Tri2 = [row1,row2]

paths :: Triangle -> [Path]
paths [] = [[]]
paths [x] = [[x]]
paths (xs:xss) = 
