-- classes can be defined using the class declaration
-- example of the equal type class 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Prelude hiding ((>=), (<=), (==))
import Data.Time.Format.ISO8601 (yearFormat)
import Distribution.Simple.Utils (xargs)
{-# HLINT ignore "Use /=" #-}

class Eq' a where 
    (==),(/=) :: a->a -> Bool
    x /= y = not (x == y)


-- types that are declared on the data and newtype are or can be made into instances of classes
-- now if we want to extend this 

class Eq' a => Ord a where 
    (<),(>),(>=),(<=) :: a->a-> Bool
    min,max           :: a -> a -> a

    min x y | x <= y = x
            | otherwise = y

    max x y  | x >= y = x
             | otherwise = y
            


