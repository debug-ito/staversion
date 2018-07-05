{-# LANGUAGE OverloadedStrings #-}
module Staversion.Internal.CommandSpec (main,spec) where

import Test.Hspec

import Staversion.Internal.Command
  ( _parseCommandStrings, Command(..)
  )
import Staversion.Internal.Query
  ( PackageSource(..), Query(..)
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "_parseCommandStrings" $ do
  it "should use SourceStackDefault if no source is specified." $ do
    (Just com) <- _parseCommandStrings ["aeson"]
    commQueries com `shouldBe` [QueryName "aeson"]
    commSources com `shouldBe` [SourceStackDefault]
  it "should use the given sources when there are some." $ do
    (Just com) <- _parseCommandStrings ["-r", "lts-10", "base"]
    commQueries com `shouldBe` [QueryName "base"]
    commSources com `shouldBe` [SourceStackage "lts-10"]
  it "should treat plain stack.yaml as QueryStackYamlDefault" $ do
    (Just com) <- _parseCommandStrings ["stack.yaml"]
    commQueries com `shouldBe` [QueryStackYamlDefault]
  it "should treat stack.yaml with directory delimiter as QueryStackYaml" $ do
    (Just com) <- _parseCommandStrings ["./stack.yaml"]
    commQueries com `shouldBe` [QueryStackYaml "./stack.yaml"]
  it "should treat *.cabal as QueryCabalFile" $ do
    (Just com) <- _parseCommandStrings ["foobar.cabal"]
    commQueries com `shouldBe` [QueryCabalFile "foobar.cabal"]
  it "should use QueryStackYamlDefault if there is no query argument" $ do
    (Just com) <- _parseCommandStrings []
    commQueries com `shouldBe` [QueryStackYamlDefault]
