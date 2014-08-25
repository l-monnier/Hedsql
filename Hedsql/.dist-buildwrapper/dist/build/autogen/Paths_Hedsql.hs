module Paths_Hedsql (
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
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/leonardmonnier/.cabal/bin"
libdir     = "/Users/leonardmonnier/.cabal/lib/Hedsql-0.1/ghc-7.6.3"
datadir    = "/Users/leonardmonnier/.cabal/share/Hedsql-0.1"
libexecdir = "/Users/leonardmonnier/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Hedsql_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Hedsql_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Hedsql_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Hedsql_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
