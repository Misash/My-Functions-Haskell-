

--DATA TYPE
data Tree a = Null | Node (Tree a) a (Tree a) deriving (Show, Eq)



--NUMBER OF ELEMENTS TREE
treeSize :: Tree a -> Integer
treeSize Null = 0
treeSize (Node i n d) = (treeSize i) + 1 + (treeSize d)


--FIND DEPTH TREE
treeDepth :: Tree a -> Integer
treeDepth Null = -1
treeDepth (Node i n d) = max (treeDepth i) (treeDepth d) + 1


--FLATTEN TREE
flattenTree::Tree a -> [a]
flattenTree Null = []
flattenTree (Node i n d) = flattenTree i ++ [n] ++ flattenTree d


--LEAVES TREE
leavesTree :: Tree a -> [a]
leavesTree Null = []
leavesTree (Node Null n Null) = [n]
leavesTree (Node i _ d) = leavesTree i ++ leavesTree d


--MAP TREE
treeMap :: (a -> b) -> (Tree a) -> (Tree b)
treeMap f Null = Null 
treeMap f (Node i n d) = Node (treeMap f i) (f n) (treeMap f d)

--SEARCHING TREE
elemTree :: Integer -> (Tree Integer) -> Bool
elemTree x Null = False 
elemTree x (Node i n d) = elemTree x i || (n == x ) || elemTree x d

--MIN N' MAX ELEM TREE
treeMaximum :: Tree Integer -> Integer
treeMaximum t = maximum $ flattenTree t

treeMinimum :: Tree Integer -> Integer
treeMinimum t = minimum $ flattenTree t




--EXAMPLES TREES

tree1 :: Tree Integer
tree1 = Node (Node (Node (Null) 2 (Null)) 3 (Null)) 5 (Node (Null) 7 (Null))

tree2:: Tree Integer 
tree2 = Node Null 1 (Node Null 2 (Node Null 3 Null))

tree3 :: Tree Integer
tree3 = Node (Node Null 1 Null) 2 (Node Null 3 Null)

main = do
    putStr "number of elems: "
    print $ treeSize tree1
    putStr "number of elems: "
    print $ treeDepth (Node Null 3 Null)
    putStr "flatten Tree: "
    print $ flattenTree tree1 
    putStr "leaves Tree: "
    print $ leavesTree tree2
    putStr "Map Tree: "
    print $ treeMap (*2) tree1
    putStr "Search Tree: "
    print $ elemTree 5 tree3 
    putStr "Mim elem Tree: "
    print $ treeMinimum tree1
    putStr "Max elem Tree: "
    print $ treeMaximum tree1


