
-- binary tree implementaion sorta 

data Tree a = Leaf a | Node (Tree a) a (Tree a )


occurs :: (Eq a) => a -> Tree a -> Bool
-- base condition 
occurs x (Leaf a) = x == a 
-- either it is at the middle, or check the left or check the right tree 
occurs x (Node leftTree middle rightTree)= x == middle || occurs x leftTree || occurs x rightTree


flatten :: Tree a -> [a]
flatten (Leaf a) = [a]
flatten (Node leftTree middle rightTree) =  flatten leftTree++ [middle] ++ flatten rightTree


-- other functions to add : insert function
-- easier to first implement the binary tree then work from the theory there :) 
