--
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- Main
--

module Main (main) where

import ReadAndProcess
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> readFileAndProcess filename
        _ -> readStdinAndProcess