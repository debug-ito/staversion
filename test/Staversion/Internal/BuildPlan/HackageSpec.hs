module Staversion.Internal.BuildPlan.HackageSpec (main,spec) where

import qualified Data.ByteString.Lazy as BSL
import System.FilePath ((</>))
import Test.Hspec

import Staversion.Internal.TestUtil (ver)

import Staversion.Internal.BuildPlan.Hackage
  ( parsePreferredVersionsJSON,
    latestVersion
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "parsePreferredVersionsJSON" $ do
  it "should parse aeson_preferred.json" $ do
    yaml_data <- BSL.readFile ("test" </> "data" </> "aeson_preferred.json")
    rvers <- either (\e -> error ("parse error: " ++ e)) return $ parsePreferredVersionsJSON yaml_data
    latestVersion rvers `shouldBe` Just (ver [1,0,2,1])
