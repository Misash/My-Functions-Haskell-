

import Data.Char (ord)


cifrar :: [Char] -> Int -> [Char]
cifrar txt n = [  fun c | c <- txt ]
    where fun c
            | c == ' ' = ' '
            | otherwise  = toEnum ((ord c) + n + 2)::Char

descifrar :: [Char] -> Int -> [Char]
descifrar txt n = [ fun c::Char  | c <- txt ]
    where fun c
                | c == ' ' = ' '
                | otherwise  = toEnum ((ord c) - n - 2)::Char

main :: IO ()
main = do 
    print $ cifrar "en todo la medida" 3
    print $ descifrar "js ytit qf rjinif" 3
    print $ cifrar "HOLA MUNDO" 5
    txt <-  return $ cifrar "HOLA MUNDO" 5
    print $ descifrar txt 5