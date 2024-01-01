module ReadAndProcess
(
    readFileAndProcess,
    readStdinAndProcess,
) where

import System.IO (stdin, hGetContents)
import Tokenization
import Cpt

readFileAndProcess :: FilePath -> IO ()
readFileAndProcess filename = do
    contents <- readFile filename
    putStrLn (show (tokenToCpt (stringToToken contents)))

readStdinAndProcess :: IO ()
readStdinAndProcess = do
    contents <- hGetContents stdin
    putStrLn (show (tokenToCpt (stringToToken contents)))