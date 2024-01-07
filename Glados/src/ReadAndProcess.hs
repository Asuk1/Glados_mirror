--
-- EPITECH PROJECT, 2024
-- Glados_mirror
-- File description:
-- ReadAndProcess
--

module ReadAndProcess
(
    readStdinAndProcess,
) where

import System.IO (isEOF, stdin, hIsEOF)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Control.Monad (unless, when)
import Tokenization
import Cpt

readStdinAndProcess :: IO ()
readStdinAndProcess = do
    emptyInput <- hIsEOF stdin
    when emptyInput $
        exitWith (ExitFailure 84)
    eof <- isEOF
    unless eof $ do
        line <- getLine
        if null line
            then putStrLn "Please write something"
            else putStrLn $ show $ tokenToCpt $ stringToToken line
        readStdinAndProcess
