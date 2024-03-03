module Divides where


divides :: Int ->Int->Bool
divides _ 0 = error "Can not divide by 0"
divides x y | x `mod` y == 0 = True
            | otherwise = False


