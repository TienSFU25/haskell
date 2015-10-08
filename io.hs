import Data.Char  
  
--main = do  
--    putStrLn "What's your first name?"  
--    firstName <- getLine  
--    putStrLn "What's your last name?"  
--    lastName <- getLine  
--    let bigFirstName = map toUpper firstName  
--        bigLastName = map toUpper lastName  
--    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?" 

--main = do   
--    line <- getLine  
--    if null line  
--        then return ()  
--        else do  
--            putStrLn $ reverseWords line  
--            main  
  
--reverseWords :: String -> String  
--reverseWords = unwords . map reverse . words  

--does this 3 times
--main = do  
--    rs <- sequence [getLine, getLine, getLine]  
--    print rs  
  
--reads from stdin
--main = do  
--    contents <- getContents  
--    putStr $ shortLinesOnly contents

--shortLinesOnly :: String -> String  
--shortLinesOnly input =   
--    let allLines = lines input  
--        --shortLines = filter (\line -> length line > 30) allLines  
--        result = unlines allLines
--    in  result  

