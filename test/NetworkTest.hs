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
import Staversion.Internal.Command (Command(..))
import Staversion.Internal.Exec (processCommand)
import Staversion.Internal.Log (defaultLogger, Logger(loggerThreshold))
import Staversion.Internal.Query
 ( PackageSource(..), ErrorMsg, Query(..), Result(..),
   resultVersionsToList
 )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_Stackage
  spec_BuildPlan
  spec_Hackage
  spec_Exec

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

quietLogger :: Logger
quietLogger = defaultLogger { loggerThreshold = Nothing }

expectRight :: String -> Either ErrorMsg a -> IO a
expectRight msg_head = either (\err -> error $ msg_head ++ err) return

isJustAnd :: Maybe a -> (a -> Bool) -> Bool
isJustAnd m p = maybe False p m

spec_BuildPlan :: Spec
spec_BuildPlan = describe "BuildPlan" $ do
  describe "loadBuildPlan from Stackage" $ do
    it "disambiguates LTS version and fetches a valid BuildPlan" $ do
      bp_man <- newBuildPlanManager "." quietLogger True
      bp <- expectRight "loadBuildPlan failed: " =<< loadBuildPlan bp_man [] (SourceStackage "lts-5")
      packageVersion bp "base" `shouldBe` Just (ver [4,8,2,0])
      packageVersion bp "bytestring" `shouldBe` Just (ver [0,10,6,0])
      packageVersion bp "conduit" `shouldBe` Just (ver [1,2,6,6])
  describe "loadBuildPlan from Hackage" $ do
    it "fetches BuildPlan for queried packages" $ do
      bp_man <- newBuildPlanManager "." quietLogger True
      bp <- expectRight "loadBuildPlan failed: " =<< loadBuildPlan bp_man ["base", "lens", "transformers"] SourceHackage
      packageVersion bp "base" `shouldSatisfy` (`isJustAnd` (>= ver [4,9,0,0]))
      packageVersion bp "lens" `shouldSatisfy` (`isJustAnd` (>= ver [4,15,1]))
      packageVersion bp "transformers" `shouldSatisfy` (`isJustAnd` (>= ver [0,5,2,0]))

spec_Hackage :: Spec
spec_Hackage = describe "BuildPlan.Hackage" $ do
  describe "fetchPreferredVersions" $ do
    it "fetches a non-empty latestVersion" $ do
      man <- makeManager
      ret <- fmap latestVersion <$> fetchPreferredVersions man "http-client"
      case ret of
       Right (Just v) -> v `shouldSatisfy` (>= ver [0,5,3,3])
       _ -> expectationFailure ("Unexpected return: " ++ show ret)

spec_Exec :: Spec
spec_Exec = describe "Exec" $ describe "processCommand" $ do
  specify "search hackage" $ do
    let comm = Command { commBuildPlanDir = ".",
                         commLogger = quietLogger,
                         commSources = [SourceHackage],
                         commQueries = [QueryName "base"],
                         commAllowNetwork = True
                       }
    [ret] <- processCommand comm
    resultIn ret `shouldBe` SourceHackage
    resultFor ret `shouldBe` QueryName "base"
    [(got_name, Just got_version)] <- resultVersionsToList <$> expectRight "processCommand error" (resultVersions ret)
    got_name `shouldBe` "base"
    got_version `shouldSatisfy` (>= ver [4,9,0,0]) 
    
    
