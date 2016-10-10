module Staversion.Internal.FormatSpec (main,spec) where

import Data.Monoid ((<>))
import Test.Hspec

import Staversion.Internal.Format (formatResultsCabal)
import Staversion.Internal.Query
  ( Result(..), PackageSource(..), Query(..)
  )

import Staversion.Internal.TestUtil (ver, rvers)
       


main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "formatResultsCabal" $ do
  it "should format a Result in a Cabal way" $ do
    let input = [ Result { resultIn = SourceStackage "lts-6.10",
                           resultFor = QueryName "hoge",
                           resultVersions = Right $ rvers [("hoge", Just $ ver [3,4,5])]
                         }
                ]
        expected = ( "------ lts-6.10\n"
                     <> "hoge ==3.4.5\n"
                     <> "\n"
                   )
    formatResultsCabal input `shouldBe` expected

