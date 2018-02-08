{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_tfl_tube (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/nuriaahmadi/Library/Haskell/bin"
libdir     = "/Users/nuriaahmadi/Library/Haskell/ghc-8.2.1-x86_64/lib/tfl-tube-0.1.0.0"
dynlibdir  = "/Users/nuriaahmadi/Library/Haskell/ghc-8.2.1-x86_64/lib/x86_64-osx-ghc-8.2.1"
datadir    = "/Users/nuriaahmadi/Library/Haskell/share/ghc-8.2.1-x86_64/tfl-tube-0.1.0.0"
libexecdir = "/Users/nuriaahmadi/Library/Haskell/libexec/x86_64-osx-ghc-8.2.1/tfl-tube-0.1.0.0"
sysconfdir = "/Users/nuriaahmadi/Library/Haskell/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tfl_tube_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tfl_tube_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tfl_tube_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tfl_tube_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tfl_tube_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tfl_tube_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
