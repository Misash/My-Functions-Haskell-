

--1

suma:: [Int] -> [Int] -> Int ->Int 
suma a b pos 
    | (pos >= (length a)) &&  (pos >= (length b)) = error "invalido!!"
    | otherwise = fun a + fun b 
        where fun x = if pos >= length x then 0 else x !! pos

--2

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted  = quicksort [a | a <- xs, a > x]
    in  biggerSorted ++ [x] ++ smallerSorted


--3

isPrime :: Int -> Bool 
isPrime k = if k > 1 then null [ x | x <- [2.. (k-1)], k `mod` x == 0] else False

primos:: Int -> [Int]
primos n = fun [] 1 0
    where fun xs x iter 
            | iter == n  = xs
            | otherwise = if (isPrime x) then fun (x:xs) (x+1) (iter+1) else fun xs (x+1) iter 
       
 
--4


diaSemana:: Int -> String
diaSemana d = 
    if d > 0 && d < 32 then
        let days = ["Sabado","Domingo","Lunes" ,"Martes" , "Miercoles","Jueves","Viernes" ] 
        in  days !! ((5+d) `mod` 7) 
    else "Ingresa un dia valido!!"


--PRUEBAS

main::IO()
main = do

putStrLn "\nEjercicio 1:" 
print (suma [1,2,3] [1,2,3,4] 3)
print (suma [1,2,3] [1,2,3] 2)

putStrLn "\nEjercicio 2:" 
print ( quicksort [4,85,1,0,0,89,1,58,6])

putStrLn "\nEjercicio 3:" 
print ( primos 1)
print ( primos 6)

putStrLn "\nEjercicio 4:" 
print ( diaSemana 18)
print ( diaSemana 55)


