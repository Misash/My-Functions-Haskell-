

--Hola Mundo
main = putStrLn "Hola Mundo"

--I/O 

main' = do
    putStrLn "what is ur name ? "
    name <- getLine
    putStrLn "What is ur age? "
    age <- getLine
    putStrLn ("Hi "++name++" ur age is "++age)
    
