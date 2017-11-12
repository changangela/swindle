module Paths_arith (
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
libdir     = "/home/angelachang/.cabal/lib/x86_64-linux-ghc-7.10.3/arith-0.1.0.0-Jaez29ZraWo5pbNmI0gREL"
datadir    = "/home/angelachang/.cabal/share/x86_64-linux-ghc-7.10.3/arith-0.1.0.0"
libexecdir = "/home/angelachang/.cabal/libexec"
sysconfdir = "/home/angelachang/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "arith_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "arith_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "arith_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "arith_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "arith_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
