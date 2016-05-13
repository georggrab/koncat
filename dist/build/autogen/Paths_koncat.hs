module Paths_koncat (
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
version = Version {versionBranch = [0,1,0,4], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/svt/.cabal/bin"
libdir     = "/home/svt/.cabal/lib/koncat-0.1.0.4/ghc-7.6.3"
datadir    = "/home/svt/.cabal/share/koncat-0.1.0.4"
libexecdir = "/home/svt/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "koncat_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "koncat_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "koncat_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "koncat_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
