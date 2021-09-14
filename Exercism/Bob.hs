
import Data.Char (ord)
--ASCII 'a' -> 97


removeSpaces :: String -> String
removeSpaces xs = [x | x <- xs , x/= ' ' && x/= '\n' && x/='\r' && x/='\t']



notLower:: String -> Bool 
notLower xs = if fun == 0 then True else False
    where fun = length [ x | x <- xs ,( (ord x > 96) && (ord x < 123) ) ]


notJustNumber::String -> Bool
notJustNumber xs =  if fun > 0 then True else False
    where fun = length [ x | x <- xs ,( (ord x > 96) && (ord x < 123) ) || ( (ord x > 64) && (ord x < 91) ) ]



responseFor :: String -> String
responseFor xs
    |  msj == "" = "Fine. Be that way!"
    |  notLower msj && notJustNumber msj  && last msj == '?' = "Calm down, I know what I'm doing!"
    |  notLower msj && notJustNumber msj =  "Whoa, chill out!"
    |  last msj == '?'  = "Sure."
    | otherwise = "Whatever."
    where msj = removeSpaces xs 
   
