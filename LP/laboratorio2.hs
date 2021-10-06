



--1

unaCifra:: Int -> Bool
unaCifra n = if (n>= 0 && n<=9) then True else False

--2

segundoNum::(Int  ,Int ,Int ) -> Int
segundoNum (a,b,c) = b 

--3 


ordenaNum:: Int -> Int -> String
ordenaNum a b
    | a > b = str_b++ " " ++ str_a 
    | otherwise = str_a ++ " " ++ str_b 
    where 
        str_a = show a
        str_b = show b
    

--4

esMultiplo:: Int -> Bool
esMultiplo n = if n `mod` 2 == 0 then True else False


--5 

mayor3::(Int  ,Int ,Int ) -> (Bool  ,Bool ,Bool )
mayor3 (a,b,c) = ( fun a , fun b , fun c)
    where fun x = if (x > 3) then True else False


--6

del_posicion_n :: [Int] -> Int -> [Int] 
del_posicion_n arr n 
    | n > length arr  = error "invalid position !!"
    | otherwise = fun arr 1
        where fun (x:xs) iter = if (iter == n) then xs else [x] ++ fun xs (iter+1)
 

--7 

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted  = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted


impares:: [Int] -> [Int]
impares xs = [ x | x <-sortedXs , x `mod` 2 /= 0  ]
    where sortedXs = quicksort xs 


--8 

busca_sub :: String -> [String] -> [String]
busca_sub str words = [ w | w <- words , take (length str) w  == str && length str > 0  ]
   
  


