

--1 

factorial:: Integer -> Integer
factorial n 
    | n == 0 = 1
    |otherwise = n* factorial(n-1)

factorial':: Integer -> Integer
factorial' n = product [1..n]

factorial'':: Integer -> Integer
factorial'' n = foldr (*) 1 [1..n]


--2

filtra_impar:: [Integer] -> [Integer]
filtra_impar xs = [ x | x<-xs , odd x]

--3

filtra_par:: [Integer] -> [Integer]
filtra_par xs = [ x | x<-xs , even x]

--4

filtraNum:: [Integer] -> String -> [Integer]
filtraNum xs "odd"  =  filter odd xs
filtraNum xs "even" =  filter even xs
filtraNum  xs op = error "invalid Operation!!"

--5


type Point = (Float ,Float)

longitud_camino:: [Point] -> Float
longitud_camino [] = error "empty List!!"
longitud_camino [x] = 0
longitud_camino (x:y:xs) = distancia x y + longitud_camino (y:xs)
    where distancia (x1,y1) (x2,y2) = sqrt((x1-x2)^2+(y1-y2)^2)
 


--6

elem_ord:: (Eq a) => a -> [a] -> Bool
elem_ord x xs = x `elem` xs   

--7 

subconjunto :: Eq a => [a] -> [a] -> Bool
subconjunto xs ys = all (`elem` ys) xs



--PRUEBAS

main::IO()
main = do

putStrLn "\nEjercicio 1:" 
print (factorial 0)
print (factorial' 5)
print (factorial'' 10)

putStrLn "\nEjercicio 2:" 
print ( filtra_impar [1..10])

putStrLn "\nEjercicio 3:" 
print ( filtra_par [1..10])

putStrLn "\nEjercicio 4:" 
print ( filtraNum [1..10] "odd" )

putStrLn "\nEjercicio 5:" 
print ( longitud_camino [(1,2),(4,6),(7,10)])

putStrLn "\nEjercicio 6:" 
print ( elem_ord 3 [1,3,5])
print ( elem_ord 2 [1,4,5])

putStrLn "\nEjercicio 7:" 
print ( subconjunto [1,3,2,3] [1,2,3])
print ( subconjunto [1,3,4,3] [1,2,3])


