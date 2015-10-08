data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a boom right)   
    | x == a = Node x boom right  
    | x < a  = Node a (treeInsert x boom) right  
    | x > a  = Node a boom (treeInsert x right)  

treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right  

-- (a -> b) -> Tree a -> Tree b.
instance Functor Tree where  
    fmap f EmptyTree = EmptyTree  
    --root node, left subtree, right subtree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)  

testTree = fmap (*4) (foldr treeInsert EmptyTree [5,7,3,2,1,7])