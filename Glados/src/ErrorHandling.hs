--
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- error_handling
--

module ErrorHandling (
    error_handling
) where

import Control.Exception (try, SomeException)
import System.IO (hFileSize, withFile, IOMode(ReadMode))
import System.Exit (exitWith, ExitCode(..))
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)

check_filepath :: FilePath -> IO Bool
check_filepath filePath = doesFileExist filePath

hasScmExtension :: FilePath -> Bool
hasScmExtension filePath = takeExtension filePath == ".scm"

check_file :: FilePath -> IO Bool
check_file filePath = do
    result <- try (withFile filePath ReadMode $ \handle -> hFileSize handle) :: IO (Either SomeException Integer)
    case result of
        Left _ -> return True
        Right size -> return (size == 0)

exitWithError :: String -> IO ()
exitWithError errMsg = do
    putStrLn errMsg
    exitWith (ExitFailure 84)

error_handling :: FilePath -> IO ()
error_handling filePath = do
    fileExists <- check_filepath filePath
    if not fileExists
        then exitWithError "Wrong filepath or the file does not exist"
        else do
            isEmpty <- check_file filePath
            if isEmpty
                then exitWithError "The file is empty."
                else do
                    isScmFile <- return $ hasScmExtension filePath
                    if not isScmFile
                        then exitWithError "File does not have the .scm extension."
                        else return ()
