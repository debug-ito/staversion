module Main (main,spec) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word)
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Test.Hspec

import Staversion.Internal.TestUtil (ver)


import Staversion.Internal.BuildPlan
  ( newBuildPlanManager,
    loadBuildPlan,
    packageVersion,
    buildPlanSource
  )
import Staversion.Internal.BuildPlan.Hackage (fetchPreferredVersions, latestVersion)
import Staversion.Internal.BuildPlan.Stackage
  ( fetchDisambiguator,
    fetchBuildPlanYAML,
    PartialResolver(..), ExactResolver(..),
    parseResolverString
  )
import Staversion.Internal.Command (Command(..))
import Staversion.Internal.Exec (processCommand)
import Staversion.Internal.Log (defaultLogger, Logger(loggerThreshold))
import Staversion.Internal.Query
 ( PackageSource(..), ErrorMsg, Query(..), Result(..),
   ResultBody(..)
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
       Right dis -> forM_ [(0,7), (1,15), (2,22), (3,22), (4,2), (5,18), (6,24), (7,8)] $ \(major, minor_min) -> do
         case dis (PartialLTSMajor major) of
          Just eresolver -> (PartialExact eresolver) `shouldBeAboveLTSMinorResolver` (major,minor_min)
          Nothing -> expectationFailure "Unexpected disambiguation error."
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

shouldBeAboveLTSMinorResolver :: PartialResolver -> (Word,Word) -> IO ()
shouldBeAboveLTSMinorResolver (PartialExact (ExactLTS got_major got_minor)) (lts_major, lts_minor_min) = do
  got_major `shouldBe` lts_major
  got_minor `shouldSatisfy` (>= lts_minor_min)
shouldBeAboveLTSMinorResolver pr _ = expectationFailure ("Unexpected PartialResolver: " ++ show pr)

shouldBeAboveLTSMinor :: PackageSource -> (Word, Word) -> IO ()
shouldBeAboveLTSMinor (SourceStackage resolver) expected_lts =
  case parseResolverString resolver of
   Just presolver -> presolver `shouldBeAboveLTSMinorResolver` expected_lts
   ret_parse -> expectationFailure ("Unexpected parse result: " ++ show ret_parse)
shouldBeAboveLTSMinor source _ = expectationFailure ("Unexpected PackageSource: " ++ show source)

spec_BuildPlan :: Spec
spec_BuildPlan = describe "BuildPlan" $ do
  describe "loadBuildPlan from Stackage" $ do
    it "disambiguates LTS version and fetches a valid BuildPlan" $ do
      bp_man <- newBuildPlanManager "." quietLogger True
      bp <- expectRight "loadBuildPlan failed: " =<< loadBuildPlan bp_man [] (SourceStackage "lts-5")
      buildPlanSource bp `shouldBeAboveLTSMinor` (5,18)
      packageVersion bp "base" `shouldSatisfy` (`isJustAnd` (>= ver [4,8,2,0]))
      packageVersion bp "bytestring" `shouldSatisfy` (`isJustAnd` (>= ver [0,10,6,0]))
      packageVersion bp "conduit" `shouldSatisfy` (`isJustAnd` (>= ver [1,2,6,6]))
  describe "loadBuildPlan from Hackage" $ do
    it "fetches BuildPlan for queried packages" $ do
      bp_man <- newBuildPlanManager "." quietLogger True
      bp <- expectRight "loadBuildPlan failed: " =<< loadBuildPlan bp_man ["base", "lens", "transformers"] SourceHackage
      buildPlanSource bp `shouldBe` SourceHackage
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
  it "should fill resultReallyIn field if necesssary" $ do
    let comm = Command { commBuildPlanDir = ".",
                         commLogger = quietLogger,
                         commSources = [SourceStackage "lts-3"],
                         commQueries = [QueryName "base"],
                         commAllowNetwork = True
                       }
    [ret] <- processCommand comm
    resultIn ret `shouldBe` SourceStackage "lts-3"
    resultFor ret `shouldBe` QueryName "base"
    case resultBody ret of
     Right (SimpleResultBody got_name (Just got_version)) -> do
       got_name `shouldBe` "base"
       got_version `shouldSatisfy` (>= ver [4,8,1,0])
     body -> expectationFailure ("Unexpected body: " ++ show body)
    case resultReallyIn ret of
     Just source -> source `shouldBeAboveLTSMinor` (3,22)
     ret_really_in -> expectationFailure ("Unexpected resultReallyIn: " ++ show ret_really_in)

  it "should search hackage" $ do
    let comm = Command { commBuildPlanDir = ".",
                         commLogger = quietLogger,
                         commSources = [SourceHackage],
                         commQueries = [QueryName "base"],
                         commAllowNetwork = True
                       }
    [ret] <- processCommand comm
    resultIn ret `shouldBe` SourceHackage
    resultFor ret `shouldBe` QueryName "base"
    resultReallyIn ret `shouldBe` Nothing
    case resultBody ret of
     Right (SimpleResultBody got_name (Just got_version)) -> do
       got_name `shouldBe` "base"
       got_version `shouldSatisfy` (>= ver [4,9,0,0])
     body -> expectationFailure ("Unexpected body: " ++ show body)
    
    
    
    
