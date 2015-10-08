--import System.IO     
    
----withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a.
--main = do
--    withFile "input.txt" ReadMode (\handle -> do  
--        contents <- hGetContents handle     
--        putStr contents)

import System.IO  
import System.Directory  
import System.Environment
import Data.List  
  
main = do        
    args <- getArgs  
    putStrLn "The arguments are:"  
    mapM putStrLn args
    let filename = (args !! 0)
    handle <- openFile filename ReadMode  
    
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let todoTasks = lines contents     
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks     
    putStrLn "These are your TO-DO items:"  
    putStr $ unlines numberedTasks  
    putStrLn "Which one do you want to delete?"     
    numberString <- getLine     
    let number = read numberString     
        newTodoItems = delete (todoTasks !! number) todoTasks     
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile filename
    renameFile tempName filename