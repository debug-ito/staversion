module Main (main,spec) where

import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Test.Hspec

import Staversion.Internal.BuildPlan.Stackage
  ( fetchDisambiguator,
    fetchBuildPlanYAML,
    PartialResolver(..), ExactResolver(..)
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = beforeAll makeManager $ do
  describe "fetchDisambiguator" $ do
    it "fetches valid disambiguator" $ \man -> do
      dis <- fetchDisambiguator man
      dis (PartialLTSMajor 2) `shouldBe` Just (ExactLTS 2 22)
  describe "fetchBuildPlanYAML" $ do
    it "fetches a LTS build plan" $ \man -> do
      raw_yaml <- fetchBuildPlanYAML man (ExactLTS 2 22)
      BSL.length raw_yaml `shouldSatisfy` (> 0)
    it "fetchces a nightly build plan" $ \man -> do
      raw_yaml <- fetchBuildPlanYAML man (ExactNightly 2016 10 20)
      BSL.length raw_yaml `shouldSatisfy` (> 0)


makeManager :: IO Manager
makeManager = newManager tlsManagerSettings

