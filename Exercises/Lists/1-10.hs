

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


        