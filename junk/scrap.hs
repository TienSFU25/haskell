import Data.Char
import System.IO  
import System.Directory

test0 :: String -> IO String
test0 filename = do
    --let filename = "fileio.hs"
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    putStrLn contents
    hClose handle
    return contents

test1 :: Int -> Int
test1 x =
    let y = test2 x
    in
    if y == Just "foo"
        then 5
    else if y == Nothing
        then 6
    else
        7

test2 :: Int -> Maybe String
test2 2 = Just "foo"
test2 1 = Nothing

x = span isLetter "abc1234 kjfalkdf2343s"