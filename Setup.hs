#!/usr/bin/env cabal
{- cabal:
build-depends: base, aeson, bytestring, Cabal, directory, filepath
-}
-- The above is a workaround for HLS ignoring `Setup.hs` files:
-- https://github.com/haskell/haskell-language-server/issues/3735
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

import Control.Monad
import Data.Aeson qualified as Ae
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromJust, isJust)
import Distribution.Compat.Prelude (isNothing)
import Distribution.PackageDescription (BuildInfo (..), GenericPackageDescription, HookedBuildInfo, Library (..), PackageDescription (..))
import Distribution.Simple (UserHooks (..), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.Flag (fromFlag)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (localPkgDescr))
import Distribution.Simple.Setup (BuildFlags (..), ConfigFlags (..), ReplFlags(..))
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

    cwd <- getCurrentDirectory

    -- Set `extra-lib-dirs` programmatically 
    -- to the absolute location `cargo` compiles to.
    -- (Unfortunately this field lives quite deep in Cabal's LocalBuildInfo record)
    let packageDescription = localPkgDescr localBuildInfo
    let packageLib = fromJust $ library packageDescription
    let libraryBuildInfo = libBuildInfo packageLib
    pure localBuildInfo
        { localPkgDescr = packageDescription 
            { library = Just packageLib
                { libBuildInfo = libraryBuildInfo
                    {
                        extraLibDirs = (cwd ++ "/target/release"): (extraLibDirs libraryBuildInfo)
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
    let verbosity = (fromFlag $ buildVerbosity flags)
    compileRust verbosity
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
    let verbosity = (fromFlag $ replVerbosity flags)
    compileRust verbosity
    replHook simpleUserHooks description localReplInfo hooks flags args


-- | Compile the Rust part of this library through Cargo.
compileRust :: Verbosity -> IO ()
compileRust verbosity = do
  putStrLn "[rust] Compiling Rust dependencies..."
  runCommand
    verbosity
    "cargo"
    [ "build"
    , "-p"
    , "haskrs_rs"
    , "--release"
    ]

-- | Print the command for debugging purposes and run it. Aborts compilation if
-- the command returns a nonzero exit code.
runCommand :: Verbosity -> String -> [String] -> IO ()
runCommand verbosity command args = do
  putStrLn ("[rust] $ " <> unwords (command : args))
  rawSystemExit verbosity command args
