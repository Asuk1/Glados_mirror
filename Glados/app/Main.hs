--
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- Main
--

module Main (main) where

import ReadAndProcess
import ErrorHandling (error_handling)

main :: IO ()
main = do
    readStdinAndProcess
