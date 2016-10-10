module Staversion.Internal.TestUtil
       ( ver, rvers
       ) where

import Data.Version (Version(..))
import Staversion.Internal.Query (PackageName, ResultVersions, resultVersionsFromList)

ver :: [Int] -> Version
ver vs = Version vs []

rvers :: [(PackageName, Maybe Version)] -> ResultVersions
rvers = resultVersionsFromList

