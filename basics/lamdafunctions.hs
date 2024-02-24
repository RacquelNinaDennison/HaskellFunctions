-- lets say that we wanted to use functions that are not that widely or frequenetly used
-- we will make use of lambda functions 
-- example

addOneToList :: [Int]->[Int]
addOneToList ls = map addOneToList' ls
                where addOneToList':: Int -> Int
                      addOneToList' x = x+1
