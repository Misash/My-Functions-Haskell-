{-# OPTIONS_GHC -Wno-overlapping-patterns #-}



compose::(a->a) -> (a->a) -> a -> a
compose f g x = f $ g x

square::Integer -> Integer
square x = x^2

inc :: Integer -> Integer
inc x = x +1

--MY MAP
myMap :: (t -> a) -> [t] -> [a]
myMap fun list = case list of
    [] -> []
    x:xs -> fun x : myMap fun xs

--MY FILTER
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter fun list = [ l | l <- list , fun l]

--MY ZIP
myZipWith :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith func (x:xs) (y:ys) = func x y : myZipWith func xs ys


repeatApply :: (Eq t1, Num t1) => (t2 -> t2) -> t1 -> t2 -> t2
repeatApply f n x = helper 0 x
    where helper i v
                | i == n = v
                | otherwise = helper (i+1) (f v)


positiveSum :: [Integer] -> Integer
positiveSum xs = foldl1 (+) (filter (>0) xs)


average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)


main :: IO ()
main = do
    print $ compose inc square 2
    print $ (inc.square) 2
    putStrLn "my Map: "
    print $ myMap not [True,False,False]
    print $ myMap (>=100) [7,105,100,-200]
    putStrLn "my Filter: "
    print $ myFilter even [1,2,3,4,5,6]
    print $ myFilter (elem 'e') ["apple", "plum", "banana", "pear"]
    putStrLn "my Zip: "
    print $ myZipWith (+) [1,2,3] [5,10,20,30,15]
    putStrLn "my repeatApply: "
    print $ repeatApply (++ " No") 5 "OH"
    print $ repeatApply (*2) 3 1
    putStrLn "Positive Sum: "
    print $ positiveSum [1,-2,-1,3]
    putStrLn "Average: "
    print $ average [1,2,3,4]






