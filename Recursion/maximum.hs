module Maximum(maximum') where 


maximum' :: (Ord a)=>[a]->a
-- if the list is empty
maximum' [] = error "List can not be empty to test"
maximum' [x] = x 
maximum' (x:xs) 
-- guard conditions on the value 
    | x > maxTail = x
    | otherwise = maxTail
    -- recursivly find the functio
   where maxTail = maximum' xs