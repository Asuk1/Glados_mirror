module Main (main) where

import Lib
import System.Environment (getArgs)

import ReadAndProcess

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> readFileAndProcess filename
        _ -> readStdinAndProcess
