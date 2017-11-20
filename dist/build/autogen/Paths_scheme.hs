<<<<<<< HEAD
module Paths_swindle (
=======
module Paths_racket (
>>>>>>> 51fa823f0a2d7e2cda37b329176a3b1afb7ef1a6
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
<<<<<<< HEAD
libdir     = "/home/angelachang/.cabal/lib/x86_64-linux-ghc-7.10.3/swindle-0.1.0.0-7GOYj87Lu9xAdiWJVrcLb5"
datadir    = "/home/angelachang/.cabal/share/x86_64-linux-ghc-7.10.3/swindle-0.1.0.0"
=======
libdir     = "/home/angelachang/.cabal/lib/x86_64-linux-ghc-7.10.3/racket-0.1.0.0-7GOYj87Lu9xAdiWJVrcLb5"
datadir    = "/home/angelachang/.cabal/share/x86_64-linux-ghc-7.10.3/racket-0.1.0.0"
>>>>>>> 51fa823f0a2d7e2cda37b329176a3b1afb7ef1a6
libexecdir = "/home/angelachang/.cabal/libexec"
sysconfdir = "/home/angelachang/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
<<<<<<< HEAD
getBinDir = catchIO (getEnv "swindle_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "swindle_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "swindle_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "swindle_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "swindle_sysconfdir") (\_ -> return sysconfdir)
=======
getBinDir = catchIO (getEnv "racket_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "racket_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "racket_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "racket_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "racket_sysconfdir") (\_ -> return sysconfdir)
>>>>>>> 51fa823f0a2d7e2cda37b329176a3b1afb7ef1a6

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
