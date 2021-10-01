

import Data.Char (ord)


anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = [ x | x <-xss , fun x == fun xs]
    where fun n = sum[ ord i | i<-n ]
        