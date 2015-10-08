import qualified Data.Map as Map  
mymap = Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")] 

isb :: (Int, Char) -> Bool
isb (x, y) = (y == 'b')

bmap = Map.filter isb $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]