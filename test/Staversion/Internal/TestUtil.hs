module Staversion.Internal.TestUtil
       ( ver, rvers, simpleResultBody
       ) where

import Data.Version (Version(..))
import Staversion.Internal.Query ( PackageName, ResultVersions, resultVersionsFromList,
                                   ResultBody(..)
                                 )

ver :: [Int] -> Version
ver vs = Version vs []

rvers :: [(PackageName, Maybe Version)] -> ResultVersions
rvers = resultVersionsFromList

simpleResultBody :: PackageName -> [Int] -> ResultBody
simpleResultBody name vs = SimpleResultBody name mversion where
  mversion = case vs of
    [] -> Nothing
    _ -> Just $ ver vs

