import Control.Monad (when)
-- understanding recursion 


count :: Int -> [Int] -> Int
count n list = go 0 list
    where
        go :: Int -> [Int] -> Int
        go result l
          | null l = result
          | head l == n = go (result + 1) (tail l)
          | otherwise = go result (tail l)