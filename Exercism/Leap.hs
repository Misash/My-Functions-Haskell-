
--Given a year, report if it is a leap year.


isLeapYear :: Int -> Bool
isLeapYear n 
    | (fun n 4) && (fun n 100) && (fun n 400) = True
    | (fun n 4) = True
    | otherwise = False 
    where fun a b = if (mod a b) == 0
                    then True else False


--Better Solution
isLeapYear' :: Int -> Bool
isLeapYear' year = divisibleBy 400 || (divisibleBy 4 && not (divisibleBy 100)) 
  where
    divisibleBy x    = year `mod` x == 0 