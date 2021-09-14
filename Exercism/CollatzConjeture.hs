



collatz :: Integer -> Maybe Integer
collatz  x = 
    let n = 0
        fun x num 
            | x == 1 = Just num
            | x < 1 = Nothing
            | x `mod` 2 == 0 = fun (x `div` 2) (num+1)
            | x `mod` 2 == 1 = fun (3*x+1) (num+1)
    in fun x n

