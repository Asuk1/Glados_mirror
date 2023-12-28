--
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- Main
--

module Main (main) where

import ReadAndProcess
import System.Environment (getArgs)
import ErrorHandling (error_handling)
import System.Exit (exitWith, ExitCode(..))

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            error_handling filename
            readFileAndProcess filename
        _ -> do
            putStrLn "Wrong number of arguments. Usage: glados <filename>"
            exitWith (ExitFailure 84)
