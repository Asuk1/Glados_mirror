{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_glados (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/tfiancette/delivery/tek3/project/Glados_mirror/Glados/.stack-work/install/x86_64-linux-tinfo6/d36e7fd94ace680130d28bff587e7001a65993501c15535bd01d12c8725da6ec/9.4.8/bin"
libdir     = "/home/tfiancette/delivery/tek3/project/Glados_mirror/Glados/.stack-work/install/x86_64-linux-tinfo6/d36e7fd94ace680130d28bff587e7001a65993501c15535bd01d12c8725da6ec/9.4.8/lib/x86_64-linux-ghc-9.4.8/glados-0.1.0.0-E5Mj3l6MejCBcJMSK41pqx"
dynlibdir  = "/home/tfiancette/delivery/tek3/project/Glados_mirror/Glados/.stack-work/install/x86_64-linux-tinfo6/d36e7fd94ace680130d28bff587e7001a65993501c15535bd01d12c8725da6ec/9.4.8/lib/x86_64-linux-ghc-9.4.8"
datadir    = "/home/tfiancette/delivery/tek3/project/Glados_mirror/Glados/.stack-work/install/x86_64-linux-tinfo6/d36e7fd94ace680130d28bff587e7001a65993501c15535bd01d12c8725da6ec/9.4.8/share/x86_64-linux-ghc-9.4.8/glados-0.1.0.0"
libexecdir = "/home/tfiancette/delivery/tek3/project/Glados_mirror/Glados/.stack-work/install/x86_64-linux-tinfo6/d36e7fd94ace680130d28bff587e7001a65993501c15535bd01d12c8725da6ec/9.4.8/libexec/x86_64-linux-ghc-9.4.8/glados-0.1.0.0"
sysconfdir = "/home/tfiancette/delivery/tek3/project/Glados_mirror/Glados/.stack-work/install/x86_64-linux-tinfo6/d36e7fd94ace680130d28bff587e7001a65993501c15535bd01d12c8725da6ec/9.4.8/etc"

getBinDir     = catchIO (getEnv "glados_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "glados_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "glados_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "glados_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "glados_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "glados_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
