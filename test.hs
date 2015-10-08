doubleMe x = x + x
doubleSmall x = if x > 100 then x else x*2

somenums = [1, 2, 3, 5, 1, 4]
morenums = [4, 3, 2, 1]
num = morenums !! 1

r = take 4 (repeat 2)

--list comprehension
lcomp = [x*2 | x <- [1..10], x*2 > 12]

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

removeNonUppercase :: [Char] -> [Char]  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]  

lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"

factorial :: (Integral somenum) => somenum -> somenum  
factorial 0 = 1  
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

--dec :: [Integer] -> [Integer]
dec :: (Integral a) => a -> a
dec x = x - 1

inc :: Integer -> Integer
inc a = a + 1

head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x

lengthy' :: (Num b) => [a] -> b  
lengthy' [] = 0  
lengthy' (_:xs) = 1 + length' xs

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]            

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs  

replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x  

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted      


--test1 :: (Integral a, Integral b, Integral cdefCLGT) => a -> a -> a
--test1 _ = 