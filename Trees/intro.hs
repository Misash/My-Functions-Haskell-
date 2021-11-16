



data Arbol a = H a
             | N a (Arbol a) (Arbol a)
             deriving (Show, Eq)



nHojas :: Arbol a -> Int
nHojas (H _)     = 1
nHojas (N x i d) = nHojas i + nHojas d

nNodos :: Arbol a -> Int
nNodos (H _)     = 0
nNodos (N x i d) = 1 + nNodos i + nNodos d 




--impresiones pre - in - posOrder

preOrder:: Arbol a -> [a]
preOrder (H z) = [z]
preOrder (N x i d) = [x] ++ preOrder i ++ preOrder d 

inOrder:: Arbol a -> [a]
inOrder (H z) = [z]
inOrder (N x i d) = inOrder i++ [x] ++ inOrder d 

postOrder:: Arbol a -> [a]
postOrder (H z) = [z]
postOrder (N x i d) = postOrder i ++ postOrder d ++ [x]


heightTree::Arbol a -> Int
heightTree (H _) = 0
heightTree  (N x i d) =  (max (heightTree i)  (heightTree d) ) + 1



findElem :: Eq a => Arbol a -> a -> Bool
findElem (H x) c =  ( x == c)
findElem (N x i d) c = findElem i c ||  ( x == c)  || findElem d c



main = do 
print $ findElem (N '8' (N '3' (H '2') (H '4')) (H '9'))  '1'
print $ findElem (N '8' (N '3' (H '2') (H '4')) (H '9'))  '8'
print $ findElem (N '8' (N '3' (H 'k') (H '4')) (H '9'))  'k'
-- print( preOrder (N 9 (N 3 (H 2) (H 4)) (H 17)) ) 
-- putStr "InOrder: "
-- print(inOrder (N 9 (N 3 (H 2) (H 4)) (H 17)) ) 
-- putStr "PostOrder:"
-- print(postOrder (N 9 (N 3 (H 2) (H 4)) (H 17)) ) 
-- putStr "Height:  "
-- print( heightTree  (N 9 (N 3 (N 2 (H 0) (H 1)) (H 4)) (N 17 (H 10) (H 18))))
