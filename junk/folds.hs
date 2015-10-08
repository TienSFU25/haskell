maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  
  
reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) []  
  
product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  
  
filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []  
  
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  
  
last' :: [a] -> a  
last' = foldl1 (\_ x -> x)  

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1  

--these 2 are same (+) and (\acc x -> acc + x)
--Prelude> scanl1 (\acc x -> acc + x) (map sqrt [1,2])
--[1.0,2.414213562373095]
--Prelude> :t scanl1
--scanl1 :: (a -> a -> a) -> [a] -> [a]
--Prelude> :t scanl1 (+)
--scanl1 (+) :: Num a => [a] -> [a]
--Prelude> :t (+)
--(+) :: Num a => a -> a -> a

data PabloE = All(Int) | Var(String) | And(PabloE, PabloE) | Or(PabloE, PabloE) | Xor(PabloE, PabloE)
               | Not(PabloE) | Advance(PabloE, Int) | MatchStar(PabloE, PabloE)
   deriving Show

data PabloS = Assign(String, PabloE) |  If (PabloE, [PabloS], [PabloS])| While (PabloE, [PabloS])
   deriving Show

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v  
--findKey key [] = Nothing  
--findKey key ((k,v):xs) = if key == k  
--                            then Just v  
--                            else findKey key xs 

findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing  

phoneBook =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ] 