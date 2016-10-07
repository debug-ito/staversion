module Staversion.Internal.ExecSpec (main,spec) where

import System.FilePath ((</>))
import Test.Hspec

import Staversion.Internal.Command (Command(..))
import Staversion.Internal.Exec (processCommand)
import Staversion.Internal.Log (defaultLogger, Logger(loggerThreshold), LogLevel(LogError))

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "processCommand" $ do
  it "s test needs to be implemented" $ True `shouldBe` False

baseCommand :: Command
baseCommand = Command { commBuildPlanDir = "test" </> "data",
                        commLogger = defaultLogger { loggerThreshold = LogError },
                        commSources = [],
                        commQueries = []
                      }


