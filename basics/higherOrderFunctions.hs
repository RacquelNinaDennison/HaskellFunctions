-- these functions allow us to apply functions to functions 

-- let's apply some function 

applyFunction :: (Int->Int->Int)->Int->Int
applyFunction f x = f x x

-- f is the function 
-- (Int->Int->Int)- this tells us about the function f's inputs 
-- Int will tell us about the second variable