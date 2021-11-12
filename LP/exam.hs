

import System.Random


adivina':: Int -> Int  -> String 
adivina' x n 
    | x < n  = "es mayor "
    | x > n  = "es menor"
    | otherwise = "exacto!!"

game :: IO ()


game = do
    print ( "\nNumero aleatorio: ")
    n <- randomRIO (1::Int, 100)
    print ( "\nNumero del usuario: ")
    x <- randomRIO (1::Int, 100)
    
    putStrLn "Tienes que adivinar un numero entre 1 y 100"
    adivina' x n





