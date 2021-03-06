{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_buffer_vis (
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

bindir     = "/Users/joes/.cabal/bin"
libdir     = "/Users/joes/.cabal/lib/x86_64-osx-ghc-8.10.4/buffer-vis-0.1.0.0-inplace-buffer-vis"
dynlibdir  = "/Users/joes/.cabal/lib/x86_64-osx-ghc-8.10.4"
datadir    = "/Users/joes/.cabal/share/x86_64-osx-ghc-8.10.4/buffer-vis-0.1.0.0"
libexecdir = "/Users/joes/.cabal/libexec/x86_64-osx-ghc-8.10.4/buffer-vis-0.1.0.0"
sysconfdir = "/Users/joes/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "buffer_vis_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "buffer_vis_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "buffer_vis_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "buffer_vis_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "buffer_vis_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "buffer_vis_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
