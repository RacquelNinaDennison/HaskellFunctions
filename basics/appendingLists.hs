-- functions that will append two lists together
-- practice in understanding how the where keyword works


lastTwo :: [Int]->[Int]->[Int]
lastTwo listOne listTwo = lastTwoList listOne ++ lastTwoList listTwo
    where lastTwoList :: [Int]->[Int]
          lastTwoList l = reverse(take 2 (reverse l))
        