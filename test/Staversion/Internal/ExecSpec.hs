module Staversion.Internal.ExecSpec (main,spec) where

import Data.Version (Version(Version))
import System.FilePath ((</>))
import Test.Hspec

import Staversion.Internal.Command (Command(..))
import Staversion.Internal.Exec (processCommand)
import Staversion.Internal.Query
  ( PackageName,
    Query(..),
    PackageSource(..),
    Result(..),
    ResultVersions,
    resultVersionsFromList,
    ErrorMsg
  )
import Staversion.Internal.Log (defaultLogger, Logger(loggerThreshold), LogLevel(LogError))

import Staversion.Internal.TestUtil (ver, rvers)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "processCommand" $ do
  specify "QueryName, SourceStackage, hit" $ do
    singleCase (SourceStackage "conpact_build_plan") (QueryName "drawille")
      (Right $ rvers [("drawille", Just $ ver [0,1,0,6])])
  specify "QueryName, SourceStackage, miss" $ do
    singleCase (SourceStackage "conpact_build_plan") (QueryName "unknown")
      (Right $ rvers [("unknown", Nothing)])
  specify "QueryName, SourceStackage, source not found" $ do
    singleCase' (SourceStackage "unknown") (QueryName "drawille") $ \got -> case got of
      Right _ -> expectationFailure "it should fail"
      Left msg -> do
        msg `shouldContain` "unknown.yaml"
        msg `shouldContain` "not found"
  specify "QueryName, SourceStackage, full-mesh" $ do
    let src2 = SourceStackage "lts-2.22_conpact"
        src7 = SourceStackage "lts-7.0_conpact"
        qc = QueryName "conduit"
        qa = QueryName "aeson"
        comm = baseCommand { commSources = [src2, src7], commQueries = [qc, qa] }
        expected = [ Result { resultIn = src2, resultFor = qc,
                              resultVersions = Right $ rvers [("conduit", Just $ ver [1,2,5])]
                            },
                     Result { resultIn = src2, resultFor = qa,
                              resultVersions = Right $ rvers [("aeson", Just $ ver [0,8,0,2])]
                            },
                     Result { resultIn = src7, resultFor = qc,
                              resultVersions = Right $ rvers [("conduit", Just $ ver [1,2,7])]
                            },
                     Result { resultIn = src7, resultFor = qa,
                              resultVersions = Right $ rvers [("aeson", Just $ ver [0,11,2,1])]
                            }
                   ]
    got <- processCommand comm
    got `shouldBe` expected
        

singleCase :: PackageSource -> Query -> Either ErrorMsg ResultVersions -> IO ()
singleCase src query exp_ret_vers = singleCase' src query (`shouldBe` exp_ret_vers)

singleCase' :: PackageSource -> Query -> (Either ErrorMsg ResultVersions -> IO a) -> IO a
singleCase' src query checker = do
  [got_ret] <- processCommand comm
  resultIn got_ret `shouldBe` src
  resultFor got_ret `shouldBe` query
  checker $ resultVersions got_ret
  where
    comm =  baseCommand { commSources = [src],
                          commQueries = [query]
                        }

baseCommand :: Command
baseCommand = Command { commBuildPlanDir = "test" </> "data",
                        commLogger = defaultLogger { loggerThreshold = Nothing },
                        commSources = [],
                        commQueries = []
                      }
