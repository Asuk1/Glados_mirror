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

import System.IO (isEOF, hIsEOF, stdin, hIsTerminalDevice)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Control.Monad (unless, when)
import Tokenization
import Cpt

readStdinAndProcess :: IO ()
readStdinAndProcess = do
    isTerminal <- hIsTerminalDevice stdin
    if isTerminal
        then readStdinInteractive
        else readStdinNonInteractive

readStdinInteractive :: IO ()
readStdinInteractive = do
    eof <- isEOF
    unless eof $ do
        line <- getLine
        processLine line
        readStdinInteractive

readStdinNonInteractive :: IO ()
readStdinNonInteractive = do
    emptyInput <- hIsEOF stdin
    when emptyInput $ do
        putStrLn "file is empty"
        exitWith (ExitFailure 84)
    content <- getContents
    processContent content

processLine :: String -> IO ()
processLine line = do
    if null line
        then putStrLn "Please write something"
        else putStrLn $ show $ tokenToCpt $ stringToToken line

processContent :: String -> IO ()
processContent content = do
    if null content
        then putStrLn "Please write something"
        else putStrLn $ show $ tokenToCpt $ stringToToken content
