{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Staversion.Internal.BuildPlan.StackYamlSpec (main,spec) where

import Data.Text (Text)
import Test.Hspec
import Text.Heredoc (here)

import Staversion.Internal.BuildPlan.StackYaml (configLocationFromText)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "configLocationFromText" $ do
  it "should load config-location field" $ do
    configLocationFromText sample `shouldBe` Right "/home/debugito/programs/git/staversion/stack.yaml"

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
