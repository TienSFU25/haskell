import qualified Data.Map as Map  
  
data LockerState = Taken | Free deriving (Show, Eq)  
  
type Code = String  
  
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code  
lockerLookup lockerNumber map =   
    case Map.lookup lockerNumber map of   
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"  
        Just (state, code) -> if state /= Taken   
                                then Right code  
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ] 

--some infix shit
infixr 5 :-:  
--infixr 6 :+:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)  
--data List a = Empty | a :+: (List a) deriving (Show, Read, Eq, Ord)  
infixr 5  .++  
(.++) :: List a -> List a -> List a   
Empty .++ ys = ys  
(x :-: xs) .++ ys = x :-: (xs .++ ys)  

data TrafficLight = Red | Yellow | Green  

-- when we're making an instance, 
-- we write instance Eq TrafficLight where. We replace the a with the actual type
instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False  

instance Show TrafficLight where  
    show Red = "Red light"
    show Yellow = "Yellow light"  
    show Green = "Green light"      

--add a class constraint to me, assuring that the type can be Eq-ed
--Maybe is TYPE CONSTRUCTOR, not a real type
instance (Eq m) => Eq (Maybe m) where  
    Just x == Just y = x == y  
    Nothing == Nothing = True  
    _ == _ = False
