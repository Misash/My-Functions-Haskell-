





isPangram :: String -> Bool
isPangram text = 
    let 
        isLetter a = a `elem` ['a'..'z'] || a `elem` ['A'..'Z']
        toLower str = [k | k <- str , case k of 
                                            'A' -> 'a'
                                            'B' -> 'b'
                                            'C' -> 'c'
                                            'D' -> 'd'
                                            'E' -> 'e'
                                            'F' -> 'f'
                                            'G' -> 'g'
                                            'H' -> 'h'
                                            'I' -> 'i'
                                            'J' -> 'j'
                                            'K' -> 'k'
                                            'L' -> 'l'
                                            'M' -> 'm'
                                            'N' -> 'n'
                                            'O' -> 'o'
                                            'P' -> 'p'
                                            'Q' -> 'q'
                                            'R' -> 'r'
                                            'S' -> 's'
                                            'T' -> 't'
                                            'U' -> 'u'
                                            'V' -> 'v'
                                            'W' -> 'w'
                                            'X' -> 'x'
                                            'Y' -> 'y'
                                            'Z' -> 'z' 
                                            otherwise -> k]
        noRep ls = case ls of 
                    [] -> []
                    [x] -> [x]
                    (x:xs) -> if isLetter x
                             then  x:[k | k <- noRep (xs) , k /=x && isLetter k]
                             else  [k | k <- noRep (xs) , k /=x && isLetter k]
                    
    in  length (noRep ( toLower text)) == 26


