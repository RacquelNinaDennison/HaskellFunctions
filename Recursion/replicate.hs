module Replicate(replicate') where 

replicate':: Int->a->[a]
replicate' 0 value = []
replicate' n value 
            | n <= 0 =[]
            | otherwise = value : replicate' (n-1) value
