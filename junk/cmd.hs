import System.Environment   
import System.Directory  
import System.IO  
import Data.List  
  
dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("add", add)  
            , ("view", view)  
            , ("remove", remove)  
            ]

main = do  
    (command:args) <- getArgs  
    let (Just action) = lookup command dispatch  
    action args  