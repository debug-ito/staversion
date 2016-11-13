module Staversion.Internal.ExecSpec (main,spec) where

import Data.Version (Version(Version))
import Data.IORef (readIORef)
import Data.List (isInfixOf)
import System.FilePath ((</>))
import Test.Hspec

import Staversion.Internal.BuildPlan (_setLTSDisambiguator)
import Staversion.Internal.Command (Command(..))
import Staversion.Internal.Exec
  ( processCommand,
    _processCommandWithCustomBuildPlanManager
  )
import Staversion.Internal.Query
  ( PackageName,
    Query(..),
    PackageSource(..),
    Result(..),
    ErrorMsg,
    ResultBody(..)
  )
import Staversion.Internal.Log (defaultLogger, _mockLogger, Logger(loggerThreshold), LogLevel(..))

import Staversion.Internal.TestUtil (ver, simpleResultBody)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_processCommand_basic
  spec_processCommand_disambiguates

spec_processCommand_basic :: Spec
spec_processCommand_basic = describe "processCommand" $ do
  specify "QueryName, SourceStackage, hit" $ do
    singleCase (SourceStackage "conpact_build_plan") (QueryName "drawille")
      (Right $ simpleResultBody "drawille" [0,1,0,6])
  specify "QueryName, SourceStackage, miss" $ do
    singleCase (SourceStackage "conpact_build_plan") (QueryName "unknown")
      (Right $ SimpleResultBody "unknown" Nothing)
  specify "QueryName, SourceStackage, source not found" $ do
    (logger, logs) <- _mockLogger
    let src = SourceStackage "unknown"
        query = QueryName "drawille"
        comm = baseCommand { commSources = [src],
                             commQueries = [query],
                             commLogger = logger { loggerThreshold = Just LogInfo }
                           }
    [got_ret] <- processCommand comm
    resultIn got_ret `shouldBe` src
    resultFor got_ret `shouldBe` query
    case resultBody got_ret of
      Right _ -> expectationFailure "it should fail"
      Left _ -> return ()
    got_logs <- readIORef logs
    let match :: (LogLevel, String) -> Bool
        match (level, msg) = (level >= LogWarn)
                             && ("unknown.yaml" `isInfixOf` msg)
                             && ("not found" `isInfixOf` msg)
    (length $ filter match got_logs) `shouldBe` 1
  specify "QueryName, SourceStackage, full-mesh" $ do
    let src2 = SourceStackage "lts-2.22_conpact"
        src7 = SourceStackage "lts-7.0_conpact"
        qc = QueryName "conduit"
        qa = QueryName "aeson"
        comm = baseCommand { commSources = [src2, src7], commQueries = [qc, qa] }
        expected = [ Result { resultIn = src2, resultReallyIn = Nothing, resultFor = qc,
                              resultBody = Right $ simpleResultBody "conduit" [1,2,5]
                            },
                     Result { resultIn = src2, resultReallyIn = Nothing, resultFor = qa,
                              resultBody = Right $ simpleResultBody "aeson" [0,8,0,2]
                            },
                     Result { resultIn = src7, resultReallyIn = Nothing, resultFor = qc,
                              resultBody = Right $ simpleResultBody "conduit" [1,2,7]
                            },
                     Result { resultIn = src7, resultReallyIn = Nothing, resultFor = qa,
                              resultBody = Right $ simpleResultBody "aeson" [0,11,2,1]
                            }
                   ]
    got <- processCommand comm
    got `shouldBe` expected
        

singleCase :: PackageSource -> Query -> Either ErrorMsg ResultBody -> IO ()
singleCase src query exp_ret_vers = singleCase' src query (`shouldBe` exp_ret_vers)

singleCase' :: PackageSource -> Query -> (Either ErrorMsg ResultBody -> IO a) -> IO a
singleCase' src query checker = do
  [got_ret] <- processCommand comm
  resultIn got_ret `shouldBe` src
  resultReallyIn got_ret `shouldBe` Nothing
  resultFor got_ret `shouldBe` query
  checker $ resultBody got_ret
  where
    comm =  baseCommand { commSources = [src],
                          commQueries = [query]
                        }

baseCommand :: Command
baseCommand = Command { commBuildPlanDir = "test" </> "data",
                        commLogger = defaultLogger { loggerThreshold = Nothing },
                        commSources = [],
                        commQueries = [],
                        commAllowNetwork = False
                      }

spec_processCommand_disambiguates :: Spec
spec_processCommand_disambiguates = describe "processCommand" $ do
  it "disambiguates a partial resolver and sets resultReallyIn field" $ do
    let comm = baseCommand { commSources = [SourceStackage "lts"],
                             commQueries = [QueryName "conduit"]
                           }
        withMockDisam bp_man = do
          _setLTSDisambiguator bp_man 4 2
          return bp_man
    [got_ret] <- _processCommandWithCustomBuildPlanManager withMockDisam comm
    resultIn got_ret `shouldBe` SourceStackage "lts"
    resultReallyIn got_ret `shouldBe` (Just $ SourceStackage "lts-4.2")
    resultFor got_ret `shouldBe` QueryName "conduit"
    resultBody got_ret `shouldBe` Right (simpleResultBody "conduit" [1,2,6,1])
