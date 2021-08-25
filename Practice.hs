
lucky ::(Integral a)=> a -> String
lucky 7 = "LUCKY NUMBER SEVEN"
lucky x = "Sorry , you're out of luck,pal!"


sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Four!"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

factorial' n = product [1..n]


charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil" 
charName  x = "no one"

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1,y1) (x2,y2) = (x1+x2,y1+y2)



--My functions to get elem on tripĺes
first :: (a,b,c) -> a
first (x,_,_) = x 

second :: (a,b,c) -> b 
second (_,y,_) = y

third :: (a,b,c) -> c
third (_,_,z) = z 

--My implementaion Head
head' :: [a] -> a 
head' [] = error "Can't call head an empty list, dummy!"
head' (x:_) = x 

--showing info about list
tell :: (Show a) => [a] -> String
tell [] = "Empty list"
tell (x:[]) = "The list has one elemet: " ++  show x 
tell (x:y:[]) = "The list has two elemets: " ++  show x ++ " and " ++ show y 
tell (x:y:_) = "This list is long : " ++ show x ++ " and "++ show y ++ " ... " 

--Lenght Fun using Pattern Matching
length' :: (Num b) => [a] -> b 
length' [] = 0
length' (_:xs) = 1 + length' xs 

