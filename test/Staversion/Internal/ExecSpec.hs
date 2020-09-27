module Staversion.Internal.ExecSpec (main,spec) where

import Data.Either (isLeft)
import Data.IORef (readIORef)
import Data.List (isInfixOf)
import System.FilePath ((</>))
import Test.Hspec

import Staversion.Internal.BuildPlan (_setLTSDisambiguator)
import Staversion.Internal.Command (Command(..), defFormatConfig)
import Staversion.Internal.Exec
  ( processCommand,
    _processCommandWithCustomBuildPlanManager
  )
import Staversion.Internal.Query
  ( PackageName,
    Query(..),
    PackageSource(..),
    ErrorMsg
  )
import Staversion.Internal.Result
  ( Result(..),
    ResultBody, ResultBody'(..),
    ResultSource(..)
  )
import Staversion.Internal.Log (defaultLogger, _mockLogger, Logger(loggerThreshold), LogLevel(..), LogEntry(..))
import Staversion.Internal.Cabal (Target(..))
import Staversion.Internal.Version (Version)


import Staversion.Internal.TestUtil (ver, simpleResultBody, verPairs)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_processCommand_basic
  spec_processCommand_disambiguates
  spec_processCommand_error

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
    (resultSourceQueried . resultIn) got_ret `shouldBe` src
    (resultSourceReal . resultIn) got_ret `shouldBe` Nothing
    resultFor got_ret `shouldBe` query
    case resultBody got_ret of
      Right _ -> expectationFailure "it should fail"
      Left _ -> return ()
    got_logs <- readIORef logs
    let match :: LogEntry -> Bool
        match (LogEntry level msg) = (level >= LogWarn)
                                     && ("unknown.yaml" `isInfixOf` msg)
                                     && ("not found" `isInfixOf` msg)
    (length $ filter match got_logs) `shouldBe` 1
  specify "QueryName, SourceStackage, full-mesh" $ do
    let src2 = SourceStackage "lts-2.22_conpact"
        src7 = SourceStackage "lts-7.0_conpact"
        qc = QueryName "conduit"
        qa = QueryName "aeson"
        comm = baseCommand { commSources = [src2, src7], commQueries = [qc, qa] }
        expected = [ Result { resultIn = ResultSource src2 (Just src2), resultFor = qc,
                              resultBody = Right $ simpleResultBody "conduit" [1,2,5]
                            },
                     Result { resultIn = ResultSource src2 (Just src2), resultFor = qa,
                              resultBody = Right $ simpleResultBody "aeson" [0,8,0,2]
                            },
                     Result { resultIn = ResultSource src7 (Just src7), resultFor = qc,
                              resultBody = Right $ simpleResultBody "conduit" [1,2,7]
                            },
                     Result { resultIn = ResultSource src7 (Just src7), resultFor = qa,
                              resultBody = Right $ simpleResultBody "aeson" [0,11,2,1]
                            }
                   ]
    processCommand comm `shouldReturn` expected
  specify "QueryCabalfile, SourceStackage" $ do
    let src = SourceStackage "lts-4.2"
        cabal_file = ("test" </> "data" </> "foobar.cabal_test")
        query = QueryCabalFile cabal_file
        comm = baseCommand { commSources = [src], commQueries = [query] }
        ret t vps = Result { resultIn = ResultSource src (Just src), resultFor = query,
                             resultBody = Right $ CabalResultBody cabal_file t vps
                           }
        expected = [ ret TargetLibrary $ verPairs [ ("base", [4,8,2,0]),
                                                    ("unordered-containers", [0,2,5,1])
                                                  ],
                     -- no result for "executable" section because it has no build-depends field.
                     ret (TargetTestSuite "spec") $ verPairs [ ("base", [4,8,2,0]),
                                                               ("staversion", []),
                                                               ("text", [1,2,2,0]),
                                                               ("filepath", [1,4,0,0]),
                                                               ("bytestring", [0,10,6,0])
                                                             ],
                     ret (TargetTestSuite "network-test") $ verPairs [ ("base", [4,8,2,0]),
                                                                       ("http-client", [0,4,26,2])
                                                                     ]
                   ]
    processCommand comm `shouldReturn` expected
  specify "QueryStackYaml, SourceStackYaml" $ do
    let stack_yaml = "test" </> "data" </> "stack" </> "stack_sample.yaml"
        source = SourceStackYaml stack_yaml
        real_source = SourceStackage "lts-4.2"
        query = QueryStackYaml stack_yaml
        comm = baseCommand { commQueries = [query],
                             commSources = [source]
                           }
        exp_cabal_file = "test" </> "data" </> "stack" </> "." </> "simple.cabal"
        expRBody pairs = CabalResultBody exp_cabal_file TargetLibrary $ verPairs pairs
        expected = Result { resultIn = ResultSource source (Just real_source),
                            resultFor = query,
                            resultBody = Right $ expRBody
                                         [ ("base", [4,8,2,0]),
                                           ("hashable", [1,2,4,0]),
                                           ("parsers", [0,12,3])
                                         ]
                          }
    got_results <- processCommand comm
    got_results `shouldBe` [expected]
  specify "QueryStackYaml (no packages), SourceStackage" $ do
    (logger, logs) <- _mockLogger
    let stack_yaml = "test" </> "data" </> "stack" </> "stack_empty_packages.yaml"
        source = SourceStackage "conpact_build_plan"
        query = QueryStackYaml stack_yaml
        comm = baseCommand { commQueries = [query],
                             commSources = [source],
                             commLogger = logger
                           }
    got <- processCommand comm
    got `shouldBe` []
    let match :: LogEntry -> Bool
        match ent = (level == LogWarn) && ("no result" `isInfixOf` msg) && ("Try --help" `isInfixOf` msg)
          where
            level = logLevel ent
            msg = logMessage ent
    got_logs <- readIORef logs
    (length $ filter match got_logs) `shouldBe` 1

singleCase :: PackageSource -> Query -> Either ErrorMsg ResultBody -> IO ()
singleCase src query exp_ret_vers = singleCase' src query (`shouldBe` exp_ret_vers)

singleCase' :: PackageSource -> Query -> (Either ErrorMsg ResultBody -> IO a) -> IO a
singleCase' src query checker = do
  [got_ret] <- processCommand comm
  (resultSourceQueried . resultIn) got_ret `shouldBe` src
  let exp_source_real = either (const Nothing) (const $ Just $ src) $ resultBody got_ret
  (resultSourceReal . resultIn) got_ret `shouldBe` exp_source_real
  resultFor got_ret `shouldBe` query
  checker $ resultBody got_ret
  where
    comm =  baseCommand { commSources = [src],
                          commQueries = [query]
                        }

baseCommand :: Command
baseCommand = Command { commBuildPlanDir = "test" </> "data" </> "build_plan_v1",
                        commLogger = defaultLogger { loggerThreshold = Nothing },
                        commSources = [],
                        commQueries = [],
                        commAllowNetwork = False,
                        commAggregator = Nothing,
                        commFormatConfig = defFormatConfig,
                        commStackCommand = "stack"
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
    (resultSourceQueried . resultIn) got_ret `shouldBe` SourceStackage "lts"
    (resultSourceReal . resultIn) got_ret `shouldBe` (Just $ SourceStackage "lts-4.2")
    resultFor got_ret `shouldBe` QueryName "conduit"
    resultBody got_ret `shouldBe` Right (simpleResultBody "conduit" [1,2,6,1])

spec_processCommand_error :: Spec
spec_processCommand_error = describe "processCommand" $ do
  it "should continue processing after IO error in reading .cabal file" $ do
    let src = SourceStackage "lts-4.2"
        comm = baseCommand { commSources = [src],
                             commQueries = [ QueryCabalFile "this_does_not_exist.cabal",
                                             QueryName "parsec"
                                           ]
                           }
    [got_cabal, got_name] <- processCommand comm
    resultFor got_cabal `shouldBe` QueryCabalFile "this_does_not_exist.cabal"
    resultBody got_cabal `shouldSatisfy` isLeft
    got_name `shouldBe` Result { resultIn = ResultSource src (Just src),
                                 resultFor = QueryName "parsec",
                                 resultBody = Right $ simpleResultBody "parsec" [3,1,9]
                               }
