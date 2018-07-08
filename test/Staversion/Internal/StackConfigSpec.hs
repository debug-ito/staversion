{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Staversion.Internal.StackConfigSpec (main,spec) where

import Data.IORef (IORef, readIORef)
import Data.Text (Text)
import System.FilePath ((</>))
import Test.Hspec
import Text.Heredoc (here)

import Staversion.Internal.Log
  ( LogEntry(..), _mockLogger, LogLevel(..), logLevel
  )
import Staversion.Internal.StackConfig
  ( StackConfig,
    configLocationFromText,
    newStackConfig,
    readProjectCabals
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "configLocationFromText" $ do
    it "should load config-location field" $ do
      configLocationFromText sample `shouldBe` Right "/home/debugito/programs/git/staversion/stack.yaml"
  spec_readProjectCabals
    

sample :: Text
sample = [here|stack-root: /home/debugito/.stack
project-root: /home/debugito/programs/git/staversion
config-location: /home/debugito/programs/git/staversion/stack.yaml
bin-path: /home/debugito/.stack/snapshots/x86_64-linux/lts-10.8/8.2.2/bin:/home/debugito/.stack/compiler-tools/x86_64-linux/ghc-8.2.2/bin:/home/debugito/.stack/programs/x86_64-linux/ghc-8.2.2/bin:/home/debugito/.plenv/shims:/home/debugito/.plenv/bin:/opt/gradle/bin:/home/debugito/.local/bin:/home/debugito/gems/bin:/home/debugito/perl5/bin:/home/debugito/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin
programs: /home/debugito/.stack/programs/x86_64-linux
compiler-exe: /home/debugito/.stack/programs/x86_64-linux/ghc-8.2.2/bin/ghc
compiler-bin: /home/debugito/.stack/programs/x86_64-linux/ghc-8.2.2/bin
compiler-tools-bin: /home/debugito/.stack/compiler-tools/x86_64-linux/ghc-8.2.2/bin
local-bin: /home/debugito/.local/bin
extra-include-dirs: 
extra-library-dirs: 
snapshot-pkg-db: /home/debugito/.stack/snapshots/x86_64-linux/lts-10.8/8.2.2/pkgdb
local-pkg-db: /home/debugito/programs/git/staversion/.stack-work/install/x86_64-linux/lts-10.8/8.2.2/pkgdb
global-pkg-db: /home/debugito/.stack/programs/x86_64-linux/ghc-8.2.2/lib/ghc-8.2.2/package.conf.d
ghc-package-path: /home/debugito/programs/git/staversion/.stack-work/install/x86_64-linux/lts-10.8/8.2.2/pkgdb:/home/debugito/.stack/snapshots/x86_64-linux/lts-10.8/8.2.2/pkgdb:/home/debugito/.stack/programs/x86_64-linux/ghc-8.2.2/lib/ghc-8.2.2/package.conf.d
snapshot-install-root: /home/debugito/.stack/snapshots/x86_64-linux/lts-10.8/8.2.2
local-install-root: /home/debugito/programs/git/staversion/.stack-work/install/x86_64-linux/lts-10.8/8.2.2
snapshot-doc-root: /home/debugito/.stack/snapshots/x86_64-linux/lts-10.8/8.2.2/doc
local-doc-root: /home/debugito/programs/git/staversion/.stack-work/install/x86_64-linux/lts-10.8/8.2.2/doc
dist-dir: .stack-work/dist/x86_64-linux/Cabal-2.0.1.0
local-hpc-root: /home/debugito/programs/git/staversion/.stack-work/install/x86_64-linux/lts-10.8/8.2.2/hpc
local-bin-path: /home/debugito/.local/bin
ghc-paths: /home/debugito/.stack/programs/x86_64-linux
|]

stackConfigForTest :: IO (StackConfig, IORef [LogEntry])
stackConfigForTest = do
  (logger, logs) <- _mockLogger
  return (newStackConfig logger, logs)

gotLogs :: LogLevel -> IORef [LogEntry] -> IO [LogEntry]
gotLogs threshold logs = do
  got_logs <- readIORef logs
  return $ filter ((>= threshold) . logLevel) got_logs

spec_readProjectCabals :: Spec
spec_readProjectCabals = describe "readProjectCabals" $ do
  let base_dir = "test" </> "data" </> "stack"
  specify "simple" $ do
    (sconf, logs) <- stackConfigForTest
    got <- readProjectCabals sconf $ Just (base_dir </> "stack_sample.yaml")
    got `shouldBe` Right [base_dir </> "." </> "simple.cabal"]
    gotLogs LogWarn logs `shouldReturn` []
