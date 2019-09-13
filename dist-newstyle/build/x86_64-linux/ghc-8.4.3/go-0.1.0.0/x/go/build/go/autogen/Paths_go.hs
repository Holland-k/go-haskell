{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_go (
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

bindir     = "/home/holland/.cabal/bin"
libdir     = "/home/holland/.cabal/lib/x86_64-linux-ghc-8.4.3/go-0.1.0.0-inplace-go"
dynlibdir  = "/home/holland/.cabal/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/holland/.cabal/share/x86_64-linux-ghc-8.4.3/go-0.1.0.0"
libexecdir = "/home/holland/.cabal/libexec/x86_64-linux-ghc-8.4.3/go-0.1.0.0"
sysconfdir = "/home/holland/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "go_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "go_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "go_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "go_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "go_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "go_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
