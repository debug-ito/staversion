module Staversion.Internal.ExecSpec (main,spec) where

import Data.Version (Version(Version))
import System.FilePath ((</>))
import Test.Hspec

import Staversion.Internal.Command (Command(..))
import Staversion.Internal.Exec (processCommand)
import Staversion.Internal.Query
  ( Query(..),
    PackageSource(..),
    Result(..),
    ResultVersions,
    resultVersionsFromList,
    ErrorMsg
  )
import Staversion.Internal.Log (defaultLogger, Logger(loggerThreshold), LogLevel(LogError))

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "processCommand" $ do
  specify "QueryName, SourceStackage, hit" $ do
    singleCase (SourceStackage "conpact_build_plan") (QueryName "drawille")
      (Right $ resultVersionsFromList [("drawille", Just $ ver [0,1,0,6])])
  specify "QueryName, SourceStackage, miss" $ do
    singleCase (SourceStackage "conpact_build_plan") (QueryName "unknown")
      (Right $ resultVersionsFromList [("unknown", Nothing)])
  specify "QueryName, SourceStackage, source not found" $ do
    singleCase' (SourceStackage "unknown") (QueryName "drawille") $ \got -> case got of
      Right _ -> expectationFailure "it should fail"
      Left msg -> do
        msg `shouldContain` "unknown.yaml"
        msg `shouldContain` "not found"

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
                        commLogger = defaultLogger { loggerThreshold = LogError },
                        commSources = [],
                        commQueries = []
                      }

ver :: [Int] -> Version
ver vs = Version vs []
