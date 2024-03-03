-- declaring types

type Pos = (Int,Int)

data Move = North|South|East|West 

move :: Move -> Pos ->Pos
move North (x,y) = (x,y+1)


moves :: [Move] -> Pos -> Pos 

moves [] p = p 
moves (m:ms) p = moves ms (move m p)

-- using foldl 
-- think about the implementation 



