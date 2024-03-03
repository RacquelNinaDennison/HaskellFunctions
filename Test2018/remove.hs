remove :: Int -> [Int]->[Int]

remove a [] = []
remove a (l:ls)| a == l = ls
               | otherwise = l: remove a ls 