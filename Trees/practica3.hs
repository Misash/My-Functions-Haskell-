

-- 1 )
mapF :: [t -> a] -> t -> [a]
mapF fs x = [ f x| f <-fs]

-- 2)



-- generate fs v = fun  [v] 0
--     where
--         fun [] _ = []
--         fun xs i =  fun (xs ++ [ (fs !! j) (xs !! i)]) (i+1)
--             where j = i `mod` k 
--                   k = length fs


gen :: [a -> a] -> [a -> a] -> a -> [a]
gen [] y x = gen y y x
gen _ [] _ = []
gen (f : fs) y x = x : gen fs y (f x)

generate :: [a -> a] -> a -> [a]
generate [] _ = []
generate f x = gen f f x



main = do
putStr " \nMapF :  "
print $ mapF [(*2) ,\x -> div x 2] 14
putStr " \ngenerate :  "
print $ take 10 (generate [(+3),(*2),(\x -> x-1)] 1)









