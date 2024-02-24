{- |
Module                  : Lecture1
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 1 of the Haskell Beginners course.

To complete exercises, you need to complete implementation and add
missing top-level type signatures. You can implement any additional
helper functions. But you can't change the names of the given
functions.

Comments before each function contain explanations and example of
arguments and expected returned values.

It's absolutely okay if you feel that your implementations are not
perfect. You can return to these exercises after future lectures and
improve your solutions if you see any possible improvements.
-}

module Lecture1
    ( makeSnippet
    , sumOfSquares
    , lastDigit
    , minmax
    , subString
    , strSum
    , lowerAndGreater
    ) where
import GHC.Base (VecElem(Int16ElemRep))

-- VVV If you need to import libraries, do it after this line ... VVV

-- ^^^ and before this line. Otherwise the test suite might fail  ^^^


-- type casting 
makeSnippet:: Int ->[Char]->[Char]
makeSnippet limit text = take limit ("Description: " ++ text) ++ "..."

{- | Implement a function that takes two numbers and finds sum of
their squares.

>>> sumOfSquares 3 4
25

>>> sumOfSquares (-2) 7
53

Explanation: @sumOfSquares 3 4@ should be equal to @9 + 16@ and this
is 25.
-}
-- DON'T FORGET TO SPECIFY THE TYPE IN HERE

sumOfSquares:: Int-> Int->Int
sumOfSquares x y = x^2 + y^2

{- | Implement a function that returns the last digit of a given number.

>>> lastDigit 42
2
>>> lastDigit (-17)
7

🕯 HINT: use the @mod@ function

-}
-- DON'T FORGET TO SPECIFY THE TYPE IN HERE
lastDigit :: (Integral a)=> a->a
lastDigit n = n `mod` 10

{- | Write a function that takes three numbers and returns the
difference between the biggest number and the smallest one.

>>> minmax 7 1 4
6

Explanation: @minmax 7 1 4@ returns 6 because 7 is the biggest number
and 1 is the smallest, and 7 - 1 = 6.

Try to use local variables (either let-in or where) to implement this
function.
-}
minmax:: Int->Int->Int->Int
minmax x y z = max x (max y z)  -min x (min y z)

subString :: Int -> Int -> [Char] -> [Char]
subString start end str
    | start < 0 || end < 0    = ""                -- Return empty string for negative start or end
    | end < start || start >= length str = ""     -- Return empty string if end < start or start is beyond the string length
    | end >= length str       = drop start str    -- If end is beyond the string length, take from start to the end of the string
    | otherwise               = take (end - start + 1) (drop start str)  -- Take the substring from start to end (inclusive)


{- | Write a function that takes a String — space separated numbers,
and finds a sum of the numbers inside this string.

>>> strSum "100    -42  15"
73

The string contains only spaces and/or numbers.
-}
strSum :: String -> Int
strSum str = counter (words str)
  where
    counter :: [String] -> Int
    counter [] = 0
    counter (x:xs) = read x + counter xs


{- | Write a function that takes a number and a list of numbers and
returns a string, saying how many elements of the list are strictly
greater than the given number and strictly lower.

>>> lowerAndGreater 3 [1 .. 9]
"3 is greater than 2 elements and lower than 6 elements"

Explanation: the list [1 .. 9] contains 9 elements: [1, 2, 3, 4, 5, 6, 7, 8, 9]
The given number 3 is greater than 2 elements (1 and 2)
and lower than 6 elements (4, 5, 6, 7, 8 and 9).

🕯 HINT: Use recursion to implement this function.
-}

-- lowerAndGreater:: Int -> [Int]->[Char]
-- lowerAndGreater n list = show n++" is greater than" ++ show numberOccurance ++ " and lower than" ++ show (length list - numberOccurances)++"elements"
--     let numberOccurance = counter n list
--     where 
--         counter ::[Int]-> Int
--         counter [] = 0
--         counter l = if take 1 l < n 
--                     then 1 ++ counter drop 1 l
--                     else 0 

lowerAndGreater :: Int -> [Int] -> String
lowerAndGreater n list =
    let numberOccurance = counter n list
    in
        show n ++ " is greater than " ++ show numberOccurance ++ " and lower than " ++ show (length list - numberOccurance) ++ " elements"
    where
        counter :: Int -> [Int] -> Int
        counter _ [] = 0
        counter x (y:ys)
            | y < x     = 1 + counter x ys  
            | otherwise = counter x ys     
