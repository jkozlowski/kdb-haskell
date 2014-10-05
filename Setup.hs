import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExit)
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple.LocalBuildInfo (
        LocalBuildInfo(..), InstallDirs(..), absoluteInstallDirs)
import System.Info (os, arch)

import Data.Maybe
    ( fromJust
    , fromMaybe
    )
import qualified Distribution.PackageDescription as PD
import Distribution.Simple
    ( Args
    , confHook
    , defaultMainWithHooks
    , postClean
    , preConf
    , simpleUserHooks
    )
import Distribution.Simple.LocalBuildInfo
    ( LocalBuildInfo
    , localPkgDescr
    )
import Distribution.Simple.Setup
    ( CleanFlags
    , ConfigFlags
    , cleanVerbosity
    , configConfigurationsFlags
    , configVerbosity
    , fromFlag
    )
import Distribution.Simple.Utils (rawSystemExit)
import System.Directory (getCurrentDirectory)

kdbLibDir :: String
kdbLibDir = "/c" ++ "/" ++ fst loc -- ++ snd loc
  where loc = case (os, arch) of
                 ("darwin", "x86_64") -> ("m32", "c.o")
                 _                    -> undefined

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
           confHook = volareConfHook
       , buildHook = \pd lbi uh bf -> kdbLibBuildHook pd lbi uh bf >> buildHook simpleUserHooks pd lbi uh bf
       }

kdbLibBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
kdbLibBuildHook pd lbi uh bf = do
  print lbi
  print bf

volareConfHook :: (PD.GenericPackageDescription, PD.HookedBuildInfo) ->
                  ConfigFlags ->
                  IO LocalBuildInfo
volareConfHook (description, buildInfo) flags = do
    localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
    let packageDescription = localPkgDescr localBuildInfo
        library = fromJust $ PD.library packageDescription
        libraryBuildInfo = PD.libBuildInfo library
    dir <- getCurrentDirectory
    return localBuildInfo {
        localPkgDescr = packageDescription {
            PD.library = Just $ library {
                PD.libBuildInfo = libraryBuildInfo {
                    PD.extraLibDirs = (dir ++ kdbLibDir):PD.extraLibDirs libraryBuildInfo
                }
            }
        }
    }


--main :: IO ()
--main = do
--  defaultMainWithHooks simpleUserHooks
--    { buildHook = (\pkg_descr lbi h flags -> (copyKdbLib pkg_descr lbi h flags) >> (buildHook simpleUserHooks pkg_descr lbi h flags))
--    }

--copyKdbLib :: PackageDescription
--           -> LocalBuildInfo
--           -> UserHooks
--           -> BuildFlags
--           -> IO ()
--copyKdbLib pkg_descr lbi _ flags = do
--    print "************************Hello1**************************"
--    let libPref = (fromFlag . buildDistPref $ flags) ++ "/build"
--    print $ "Copying " ++ kdbLibDir ++ " to " ++ libPref
--    -- rawSystemExit (fromFlag $ buildVerbosity flags) "cp" [kdbLibDir, libPref]

