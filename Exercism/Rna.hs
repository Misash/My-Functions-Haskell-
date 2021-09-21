



toRNA :: String -> Either Char String
toRNA xs = if length txt == 0 then a else b 
    where txt = [x | x <- xs , x /= 'G' && x /= 'C' && x /= 'T' &&  x/= 'A']
          a = Right [rna | x <- xs , let rna = case x of 'G' -> 'C'
                                                         'C' -> 'G'
                                                         'T' -> 'A'
                                                         'A' -> 'U']
          b = Left (head txt)
       



