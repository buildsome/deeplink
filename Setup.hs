import           Distribution.PackageDescription ( PackageDescription )
import           Distribution.Simple
    ( Args
    , UserHooks(postInst, postCopy)
    , simpleUserHooks
    , defaultMainWithHooks
    )
import           Distribution.Simple.InstallDirs ( InstallDirs(prefix), fromPathTemplate )
import           Distribution.Simple.LocalBuildInfo
    ( LocalBuildInfo(installDirTemplates) )
import           Distribution.Simple.Setup
    ( InstallFlags(installVerbosity), CopyFlags(copyVerbosity), fromFlagOrDefault, Flag )
import           Distribution.Simple.Utils ( installDirectoryContents )
import           Distribution.Verbosity (Verbosity)
import qualified Distribution.Verbosity as Verbosity
import           System.Directory
import           System.FilePath ( (</>) )

import           Prelude

common :: (a -> Flag Verbosity) -> a -> b -> LocalBuildInfo -> IO ()
common getVerbosity flags _pkgDesc localBuildInfo =
    do
        cwd <- getCurrentDirectory
        let installPrefix = fromPathTemplate $ prefix $ installDirTemplates localBuildInfo
        let verbosity = fromFlagOrDefault Verbosity.normal (getVerbosity flags)
        installDirectoryContents verbosity (cwd </> "include") (installPrefix </> "include")
        return ()

postInstHook :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postInstHook _args = common installVerbosity

postCopyHook :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postCopyHook _args = common copyVerbosity

hooks :: UserHooks
hooks =
    simpleUserHooks
    { postInst = postInstHook -- for Cabal <1.20, will be deprecated later
    , postCopy = postCopyHook -- for Cabal >=1.20
    }

main :: IO ()
main = defaultMainWithHooks hooks
