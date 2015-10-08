--data Bool               = False | True
data Color              = Red | Green | Blue | Indigo | Violet
data Point a            = Pt a a

test2 :: (Integer, Integer) -> Integer
test2 (x, y) = x + y

inc :: Integer -> Integer
inc x = x + 1

bomaymap                     :: ((a->b) -> ([a] -> [b]))
bomaymap f  []               =  []
bomaymap f (x:xs)            =  (f x) : (bomaymap f xs)

--before fat arrow = CONSTRAINTS
test3 :: (Num k, Ord k, Num a) => [a] -> k
test3 b = 11

applyTwice :: (a -> a) -> (a -> a)  
applyTwice f x = f (f x)  

zipWith' :: (a -> (b -> c)) -> ([a] -> ([b] -> [c]))  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  

add :: Int -> Int -> Int
add x y = x + y

chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)  

--some higher order shit

numLongChains :: Int  
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs  

elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = cond False (ys)
    where cond = foldl (\acc x -> if x == y then True else acc)  