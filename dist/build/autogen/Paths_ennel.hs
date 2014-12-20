module Paths_ennel (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "C:\\Users\\Siddharth\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Siddharth\\AppData\\Roaming\\cabal\\ennel-0.1.0.0\\ghc-7.6.3"
datadir    = "C:\\Users\\Siddharth\\AppData\\Roaming\\cabal\\ennel-0.1.0.0"
libexecdir = "C:\\Users\\Siddharth\\AppData\\Roaming\\cabal\\ennel-0.1.0.0"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "ennel_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ennel_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ennel_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ennel_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)