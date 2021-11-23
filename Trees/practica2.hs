{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


--AARON MISASH APAZA COAQUIRA


data Tree a = Node (Tree a) a (Tree a) | Nil deriving (Show, Eq)

t::Tree Int
t = Node (Node (Node Nil 3 Nil) 1 (Node Nil 5 Nil)) 0 (Node Nil 2 (Node Nil 4 Nil))


-- A )

foldTree :: (p -> t -> p -> p) -> p -> Tree t -> p
foldTree g e t = fun t
    where 
        fun Nil  = e 
        fun (Node i x d) = g (fun i) x (fun d) 

--B )
mapTree::(a->b)->Tree a -> Tree b 
mapTree f t = fun t 
    where 
        fun Nil = Nil 
        fun (Node i x d) = Node (fun i) (f x) (fun d)

--C )


maxtree :: Tree a1 -> a2
maxtree Nil = error "maxTree undefined for the empty tree"
maxTree (Node i x d) = foldTree g 0 (Node i x d)
    where g a b c = max a  (max b c ) 



main = do 
putStrLn " \nfoldTree :  "
print $ foldTree (\x y z -> x +y+z) 0 t 
putStrLn " \nmapTree :  "
print $ mapTree (*2) t
putStrLn " \nmaxTree :  "
print $ maxTree t 





