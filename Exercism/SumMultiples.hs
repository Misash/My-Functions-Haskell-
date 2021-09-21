


isMultiple:: [Integer] -> Integer -> Bool
isMultiple list n = if fun > 0 then True else False
    where fun = length [ i | i <- list , (n `mod` i)  == 0]


sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum[ x | x <- [1..limit], x < limit && fun x ]
    where fun n = isMultiple ns n
          ns = [ i | i<-factors , i /= 0]
  




