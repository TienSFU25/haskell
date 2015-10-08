--
-- A Haskell grep prototype (egrep RE syntax).
-- Demonstrating parallel bit stream algorithms 

module Main (main) where 

import CanonicalRE
import Nullable
import REparse
import RunPablo
import REcompile
import Data.List

import System.Environment
import System.Console.GetOpt
import System.IO (stdout,stderr,hPutStr,hPutStrLn)
import System.Exit (ExitCode(..),exitWith)
import Control.Monad
--
-- Search for matching lines in a list of lines
search_all :: (RE, [String]) -> [String]
search_all(re, [])  = []
search_all(re, line1:more)
  | searchRE(re, line1 ++ "\n") == Bitstream 0    = search_all(re, more)
  | otherwise                             = line1:(search_all(re, more))



header = "Usage: hgrep (-h | -v | [-c] regexp file)"
version = "hgrep 0.8"

data GrepFlags = CountLines | Help | Version deriving Eq
hgrepOptions :: [OptDescr GrepFlags]
hgrepOptions =
       [Option ['c'] ["count"]   (NoArg CountLines)  "Count the number of matching lines.",
        Option ['h'] ["help"]    (NoArg Help)        "Display help and exit.",
        Option ['v'] ["version"] (NoArg Version)     "Show the version number and exit."]

usageMsg = usageInfo header hgrepOptions

main = do
  hgrepArgs <- getArgs
  let (opts, args, errs) = getOpt Permute hgrepOptions hgrepArgs

  when (not (null errs)) $ do
       hPutStrLn stderr "Errors:"
       mapM_ (hPutStrLn stderr) errs
       exitWith (ExitFailure 1)
  when (opts == [Version]) $ do
       hPutStrLn stderr version
       exitWith ExitSuccess
  when (opts == [Help]) $ do
       hPutStrLn stderr usageMsg
       exitWith ExitSuccess
  when (length(args) /= 2) $ do
       hPutStrLn stderr usageMsg
       exitWith (ExitFailure 1)
  let [regexp, srcfile] = args
  case parseRE(regexp) of
         (ParseSuccess r) -> do
            srcText <- readFile srcfile
            let r1 = simplify(removeNullableSuffix(removeNullablePrefix(r)))
                matches = search_all(r1, lines srcText)
	    if (opts == [CountLines]) then do
                hPutStrLn stdout (show (length matches))
            else do
                hPutStr stdout (unlines matches)
            exitWith ExitSuccess
         (ParseFailure m) -> do
            hPutStrLn stderr ("Bad regexp: " ++ m)
            exitWith (ExitFailure 1)

