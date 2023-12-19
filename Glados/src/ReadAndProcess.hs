module ReadAndProcess
(
    readFileAndProcess,
    readStdinAndProcess,
) where

import System.IO (stdin, hGetContents)
import Tokenization

readFileAndProcess :: FilePath -> IO ()
readFileAndProcess filename = do
    contents <- readFile filename
    putStrLn (show (stringToToken contents))

readStdinAndProcess :: IO ()
readStdinAndProcess = do
    contents <- hGetContents stdin
    putStrLn (show (stringToToken contents))