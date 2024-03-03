
merge :: [Int]->[Int]->[Int]

merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (l:ls) (x:xs) | x < l = x : merge (l:ls) xs
                    | x > l = l : merge ls (x: xs)
                    | otherwise = x:(l : merge ls xs )



