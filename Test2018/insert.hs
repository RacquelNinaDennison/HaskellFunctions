insert :: Int ->[Int]->[Int]


insert a [] = [a]
insert a (l:ls) | a < l = a :(l:ls)
                | otherwise= l: insert a ls

