#!/usr/bin/env cabal
{- cabal:
build-depends: base, aeson, bytestring, Cabal, directory, filepath
-}
-- The above is a workaround for HLS ignoring `Setup.hs` files:
-- https://github.com/haskell/haskell-language-server/issues/3735
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Aeson qualified as Ae
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromJust, isJust)
import Distribution.Compat.Prelude (isNothing)
import Distribution.PackageDescription (BuildInfo (..), GenericPackageDescription, HookedBuildInfo, Library (..), PackageDescription (..))
import Distribution.Simple (UserHooks (..), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.Flag (fromFlag)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (localPkgDescr))
import Distribution.Simple.Setup (BuildFlags (..), ConfigFlags (configVerbosity), ReplFlags (..))
import Distribution.Simple.Utils (rawSystemExit, rawSystemStdout)
import Distribution.Types.InstalledPackageInfo (InstalledPackageInfo)
import Distribution.Verbosity (Verbosity)
import GHC.Generics (Generic)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory)
import System.Environment (getEnv, lookupEnv)
import System.FilePath ((</>))

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { confHook = rustConfHook
      , buildHook = rustBuildHook
      , replHook = rustReplHook
      }

rustConfHook
  :: (GenericPackageDescription, HookedBuildInfo)
  -> ConfigFlags
  -> IO LocalBuildInfo
rustConfHook (description, buildInfo) flags = do
    localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
    let verbosity = fromFlag (configVerbosity flags)

    compileRust verbosity

    let packageDescription = localPkgDescr localBuildInfo
    let packageLib = fromJust $ library packageDescription
    let libraryBuildInfo = libBuildInfo packageLib

    libDir <- findLibDir verbosity
    pure
        localBuildInfo
        { localPkgDescr =
            packageDescription
                { library =
                    Just
                    packageLib
                        { libBuildInfo =
                            libraryBuildInfo
                            { extraLibDirs = libDir : extraLibDirs libraryBuildInfo
                            , -- Cabal is supposed to do this for us, but it
                                -- doesn't seem to work right now. Without
                                -- setting the RPATH here any binary that links
                                -- against this library won't be able to find
                                -- the Rust library
                                ldOptions = ("-Wl,-rpath=" <> libDir) : ldOptions libraryBuildInfo
                            }
                        }
                }
        }


-- | Build hook that reruns the Cargo build every time this package's files or
-- any of the files listed under `extra-source-files` change.
rustBuildHook
  :: PackageDescription
  -> LocalBuildInfo
  -> UserHooks
  -> BuildFlags
  -> IO ()
rustBuildHook description localBuildInfo hooks flags = do
    compileRust (fromFlag $ buildVerbosity flags)
    buildHook simpleUserHooks description localBuildInfo hooks flags

-- | Like 'rustBuildHook', but for 'cabal repl'.
rustReplHook
  :: PackageDescription
  -> LocalBuildInfo
  -> UserHooks
  -> ReplFlags
  -> [String]
  -> IO ()
rustReplHook description localReplInfo hooks flags args = do
    compileRust (fromFlag $ replVerbosity flags)
    replHook simpleUserHooks description localReplInfo hooks flags args


-- | Compile the Rust part of this library through Cargo.
compileRust :: Verbosity -> IO ()
compileRust verbosity = do
  putStrLn "[rust] Compiling Rust dependencies..."
  runCommand
    verbosity
    "cargo"
    [ "build"
    , "--release"
    ]

-- | Print the command for debugging purposes and run it. Aborts compilation if
-- the command returns a nonzero exit code.
runCommand :: Verbosity -> String -> [String] -> IO ()
runCommand verbosity command args = do
  putStrLn ("[rust] $ " <> unwords (command : args))
  rawSystemExit verbosity command args

-- | 'runCommand', except it returns the command's output.
runCommandStdout :: Verbosity -> String -> [String] -> IO LBS.ByteString
runCommandStdout verbosity command args = do
  putStrLn ("[rust] $ " <> unwords (command : args))
  rawSystemStdout verbosity command args

-- | Find the directory Cargo will output the release binary to.
findLibDir :: Verbosity -> IO FilePath
findLibDir verbosity = do
  putStrLn "[rust] Finding Rust workspace root..."
  CargoManifest targetDir <-
    runCommandStdout
      verbosity
      "cargo"
      [ "metadata"
      , "--format-version=1"
      , "--no-deps"
      ]
      >>= Ae.throwDecode

  let libDir = targetDir </> "release"
  putStrLn ("[rust] Using '" <> libDir <> "'")

  exists <- doesDirectoryExist libDir
  unless exists $ fail "[rust] The target directory doesn't exist even though it should. Aborting..."

  pure libDir

newtype CargoManifest = CargoManifest {targetDir :: FilePath}
instance Ae.FromJSON CargoManifest where
  parseJSON = Ae.withObject "CargoManifest" $ \obj ->
    CargoManifest <$> obj Ae..: "target_directory"
