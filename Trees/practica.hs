



--AARON MISASH APAZA COAQUIRA



data Arbol a =  H a | N a (Arbol a) (Arbol a) deriving (Show, Eq)


--1 

inOrder:: Arbol Char -> [Char]
inOrder (H x) = [x]
inOrder (N x i d) = inOrder i  ++ [x]  ++ inOrder d 


--2


preOrder:: Arbol  a -> [a]
preOrder (H x) = [x]
preOrder (N x i d) = [x] ++ preOrder i ++ preOrder d 

postOrder:: Arbol a -> [a]
postOrder (H x) = [x]
postOrder (N x i d) = postOrder i ++ postOrder d ++ [x]


--3


data Tree  a =  Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

isSameTree :: Eq a => Tree a -> Tree a -> Bool
isSameTree  Empty Empty = True
isSameTree   _  Empty   = False 
isSameTree  Empty  _   = False 
isSameTree  (Node x i d) (Node y l r) = (x == y) && isSameTree i l && isSameTree d r


--4


data ArBinEA a = Vacio | Hoja a | NoEA (Char ,ArBinEA a ,ArBinEA a)  deriving (Eq ,Show)

opera:: Float -> Char -> Float -> Float
opera a '+' b = a + b  
opera a '*' b = a * b  
opera a '-' b = a - b  
opera a '/' b = a / b  

solveTree:: ArBinEA Float -> Float
solveTree Vacio = 0.0
solveTree (Hoja x) =  x
solveTree (NoEA (c ,i ,d) ) = opera  (solveTree i)  (c)  (solveTree d)
  


ea::ArBinEA Float
ea = NoEA ('+', NoEA ('*', Hoja 10, Hoja 5), Hoja 7)




main = do 
putStrLn " 1 :  "
print $ inOrder (N '8' (N '3' (H '2') (H '4')) (H '9'))
putStrLn " 2 :  "
print $ preOrder (N 9 (N 3 (H 2) (H 4)) (H 17))
print $ postOrder (N 9 (N 3 (H 2) (H 4)) (H 17))
putStrLn " 3 :  "
print $ isSameTree (Node 4 (Empty) (Node 5 (Empty) (Empty)) ) (Node 4 (Empty) (Empty) )
putStrLn " 4 :  "
print $ solveTree ea





