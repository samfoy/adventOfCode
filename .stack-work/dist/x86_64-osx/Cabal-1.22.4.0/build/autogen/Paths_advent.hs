module Paths_advent (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/sam/scm/advent/.stack-work/install/x86_64-osx/lts-3.16/7.10.2/bin"
libdir     = "/Users/sam/scm/advent/.stack-work/install/x86_64-osx/lts-3.16/7.10.2/lib/x86_64-osx-ghc-7.10.2/advent-0.1.0.0-LWQVGYLqmp53SwWBX5lE5g"
datadir    = "/Users/sam/scm/advent/.stack-work/install/x86_64-osx/lts-3.16/7.10.2/share/x86_64-osx-ghc-7.10.2/advent-0.1.0.0"
libexecdir = "/Users/sam/scm/advent/.stack-work/install/x86_64-osx/lts-3.16/7.10.2/libexec"
sysconfdir = "/Users/sam/scm/advent/.stack-work/install/x86_64-osx/lts-3.16/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "advent_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "advent_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "advent_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "advent_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "advent_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
