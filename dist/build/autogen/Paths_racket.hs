module Paths_swindle (
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

bindir     = "/home/angelachang/.cabal/bin"
libdir     = "/home/angelachang/.cabal/lib/x86_64-linux-ghc-7.10.3/swindle-0.1.0.0-G7Pnbjjbtsi2VTjCWTQNvY"
datadir    = "/home/angelachang/.cabal/share/x86_64-linux-ghc-7.10.3/swindle-0.1.0.0"
libexecdir = "/home/angelachang/.cabal/libexec"
sysconfdir = "/home/angelachang/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "swindle_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "swindle_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "swindle_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "swindle_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "swindle_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
