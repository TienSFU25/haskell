module Shapes   
( Point(..)  
, Shape
, surface  
, nudge
, Person    
) where  

data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)  

surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)  

s1 = surface $ Circle (Point 10 20) 10

nudge :: Shape -> Float -> Float -> Shape  
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r  
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))  

--data Person = Person String String Int Float String String deriving (Show)  

--deriving some default type classes

data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int 
                     } deriving (Show, Eq, Read)

data Car a b c = Car { company :: a  
                     , model :: b  
                     , year :: c   
                     } deriving (Show)  

--type synonym
type Homo = Person

--tellCar :: Car -> String  
tellCar :: (Show a) => Car String String a -> String  
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y  

data Vector a = Vector a a a deriving (Show)  
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n  