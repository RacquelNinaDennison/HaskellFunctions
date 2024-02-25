module QuickSort(quickSort) where 

quickSort :: (Ord a) => [a]->[a]
quickSort [] = []
quickSort (x:xs) = 
    let smallerListValues = quickSort [a| a <- xs, a <= x]
        biggerListValues = quickSort [a | a <- xs, a > x]
    in smallerListValues ++ [x] ++ biggerListValues


