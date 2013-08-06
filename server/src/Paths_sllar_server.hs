module Paths_sllar_server (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/sergey/Library/Haskell/ghc-7.4.2/lib/sllar-server-0.0.0.1/bin"
libdir     = "/Users/sergey/Library/Haskell/ghc-7.4.2/lib/sllar-server-0.0.0.1/lib"
datadir    = "/Users/sergey/Library/Haskell/ghc-7.4.2/lib/sllar-server-0.0.0.1/share"
libexecdir = "/Users/sergey/Library/Haskell/ghc-7.4.2/lib/sllar-server-0.0.0.1/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "sllar_server_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sllar_server_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "sllar_server_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sllar_server_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
