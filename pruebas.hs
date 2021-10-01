-- myLast :: [a] -> a 
-- myLast list = len 
--     where len = last list 


sum':: Integer -> Integer -> Integer
sum' x y = x + y

--Construya una función que retorna el n-ésimo elemento de la secuencia de Fibonacci.
fibonacci:: Integer -> Integer
fibonacci n 
    | n < 2 = n
    | otherwise = fibonacci (n-1) + fibonacci (n-2);



-- Escribir una función que borre la primera ocurrencia de un entero de una lista:
-- delete :: Int -> [Int] -> [Int]
-- Por ejemplo: delete 2 [1,2,3,2,1] = [1,3,2,1]


delete:: Int -> [Int] -> [Int]
delete n (x:xs) = if x == n  then xs  else x:delete n xs 


-- -Cree una función que dada una lista de números retorna la lista de los
-- impares (puede usar la función predefinida odd).

impares::[Integer] -> [Integer]
impares xs = [ x | x <-xs , x `mod` 2 /=0 ]



-- -Escribir una función para calcular el mínimo de una lista:

mini :: [Int] -> Int
mini [] = error "empty list!!"
mini [x] = x
mini (x:y:xs)
    | x > y = mini (y:xs)
    | otherwise = mini (x:xs)

















main::IO()

main = do

let a = 2
let b = 3
let arr = [1,2..10]

--putStrLn ":" 

print (last arr)









