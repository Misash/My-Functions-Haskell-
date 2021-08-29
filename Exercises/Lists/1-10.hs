

--Problem 1
--Find the last element of a list

myLast :: [a] -> a 
myLast [] = error "Empty list , dummy!"
myLast [x] = x
myLast (_:x) = myLast x

myLast' xs = last xs  --Prelude function

--Problem 2
--Find the last but one element of a list 

--Solution with Pattern Matching
myButLast :: [a] -> a 
myButLast [] = error "Empyt List , dummy!"
myButLast [x] = error "Too feew elements"
myButLast [x,y] = x
myButLast (_:xs) = myButLast xs 

--Solution with Prelude Functions
myButLast' xs = last (init xs)

--Solution with Guards
myButLast'' :: [a] -> a 
myButLast'' list 
    | len < 2 = error "Too few elemnts!"
    | len >= 2 = reverse list !!1
    where len = length list 

--Solution with Let 
myButLast''' :: [a] -> a 
myButLast''' list =
    let len  = last (init list) 
    in  len 

--Problem 3 
-- Find the K'th element of a list

--Solution with prelude 
elementAt :: [a] -> Int -> a 
elementAt xs n = xs !! (n-1)

--Solution guards 
elementAt' :: [a] -> Int -> a 
elementAt' (x:_) 1 = x 
elementAt' (_:xs) k = elementAt' xs (k-1)
elementAt' _ _  = error "Index out of bounds"

--Problem 4
--Find the number of elements of a list 
mylength :: [a] -> Int 
mylength xs = sum [1 | x <- xs ]

--using case 
mylength' :: [a] -> Int 
mylength' xs = case xs of [] -> 0
                          (_:xs) -> 1 + mylength' xs

--Problem 5
-- Reverse a list 
myReverse :: [a] -> [a]
myReverse 