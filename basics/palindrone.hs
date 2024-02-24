{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
import Data.Char (toLower)

isPalindrome :: [Char] -> Bool
isPalindrome word = 
    let cleanedWord = map toLower $ filter (/= ' ') word
    in
    if even (length cleanedWord)
        then 
            let firstHalf = take (length cleanedWord `div` 2) cleanedWord
                secondHalf = drop (length cleanedWord `div` 2) cleanedWord
            in firstHalf == secondHalf
    else False

-- getting the user input 
main :: IO ()
main = do
    putStrLn "Please enter a word to test (type 'exit' to quit):"
    input <- getLine
    if input /= "exit"
        then do
            putStrLn ("Is \"" ++ input ++ "\" a palindrome? " ++ show (isPalindrome input))
            main  -- Recursive call to keep the loop going
        else
            putStrLn "Exiting..."