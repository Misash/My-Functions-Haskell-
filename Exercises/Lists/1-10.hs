

--Problem 1
--Find the last element of a list

myLast :: [a] -> a 
myLast [] = error "Empty list , dummy!"
myLast [x] = x
myLast (_:x) = myLast x

myLast' xs = last xs  --Prelude function

--Problem 2
--Find the last but one element of a list 

myButLast :: [a] -> a 
    myButLast [] = error "Empyt Lisr , dummy!"
    myButLast [x,y] = y 
    myButLast (_:xs) = myButLast xs 