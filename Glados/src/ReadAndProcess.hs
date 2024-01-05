module ReadAndProcess
(
    readStdinAndProcess,
) where

import System.IO (isEOF)
import Tokenization
import Cpt

readStdinAndProcess :: IO ()
readStdinAndProcess = do
    eof <- isEOF
    if eof
        then return ()
        else do
            line <- getLine
            putStrLn (show (tokenToCpt (stringToToken line)))
            readStdinAndProcess
