

--Hola Mundo
-- main = putStrLn "Hola Mundo"

--I/O 

-- main = do
--     putStrLn "what is ur name ? "
--     name <- getLine
--     putStrLn "What is ur age? "
--     age <- getLine
--     putStrLn ("Hi "++name++" ur age is "++age)
    
main1 = do
    foo <- putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")

main2 = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main2

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

main3 = do
    return ()
    return "HAHAHA"
    line <- getLine
    return "BLAH BLAH BLAH"
    return 4
    putStrLn line


main4 = do  putStr "Hey, "
            putStrLn "Andy!"
            putStr "I'm "