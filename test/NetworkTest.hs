module Main (main,spec) where

import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Test.Hspec

import Staversion.Internal.TestUtil (ver)


import Staversion.Internal.BuildPlan
  ( newBuildPlanManager,
    loadBuildPlan,
    packageVersion
  )
import Staversion.Internal.BuildPlan.Hackage (fetchPreferredVersions, latestVersion)
import Staversion.Internal.BuildPlan.Stackage
  ( fetchDisambiguator,
    fetchBuildPlanYAML,
    PartialResolver(..), ExactResolver(..)
  )
import Staversion.Internal.Log (defaultLogger, Logger(loggerThreshold))
import Staversion.Internal.Query (PackageSource(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_Stackage
  spec_BuildPlan
  spec_Hackage

spec_Stackage:: Spec
spec_Stackage = describe "BuildPlan.Stackage" $ beforeAll makeManager $ do
  describe "fetchDisambiguator" $ do
    it "fetches valid disambiguator" $ \man -> do
      e_dis <- fetchDisambiguator man
      case e_dis of
       Left err -> expectationFailure ("should not be Left: " ++ err)
       Right dis -> dis (PartialLTSMajor 2) `shouldBe` Just (ExactLTS 2 22)
  describe "fetchBuildPlanYAML" $ do
    it "fetches a LTS build plan" $ \man -> do
      raw_yaml <- fetchBuildPlanYAML man (ExactLTS 2 22)
      BSL.length raw_yaml `shouldSatisfy` (> 0)
    it "fetchces a nightly build plan" $ \man -> do
      raw_yaml <- fetchBuildPlanYAML man (ExactNightly 2016 10 20)
      BSL.length raw_yaml `shouldSatisfy` (> 0)

makeManager :: IO Manager
makeManager = newManager tlsManagerSettings

spec_BuildPlan :: Spec
spec_BuildPlan = describe "BuildPlan" $ do
  describe "loadBuildPlan from Stackage" $ do
    it "disambiguates LTS version and fetches a valid BuildPlan" $ do
      let logger = defaultLogger { loggerThreshold = Nothing }
      bp_man <- newBuildPlanManager "." logger True
      bp <- loadBuildPlan bp_man (SourceStackage "lts-5") >>= \ret -> case ret of
        Right bp -> return bp
        Left error_msg -> error ("loadBuildPlan failed: " ++ error_msg)
      packageVersion bp "base" `shouldBe` Just (ver [4,8,2,0])
      packageVersion bp "bytestring" `shouldBe` Just (ver [0,10,6,0])
      packageVersion bp "conduit" `shouldBe` Just (ver [1,2,6,6])

spec_Hackage :: Spec
spec_Hackage = describe "BuildPlan.Hackage" $ do
  describe "fetchPreferredVersions" $ do
    it "fetches a non-empty latestVersion" $ do
      man <- makeManager
      ret <- fmap latestVersion <$> fetchPreferredVersions man "http-client"
      case ret of
       Right (Just v) -> v `shouldSatisfy` (>= ver [0,5,3,3])
       _ -> expectationFailure ("Unexpected return: " ++ show ret)
