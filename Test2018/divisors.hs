import Divides 

divisors :: Int -> [Int]

divisors n  = [x | x <- [1..n], divides n x]