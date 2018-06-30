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
