module Staversion.Internal.TestUtil
       ( ver, simpleResultBody
       ) where

import Data.Version (Version(..))
import Staversion.Internal.Query ( PackageName,
                                   ResultBody(..)
                                 )

ver :: [Int] -> Version
ver vs = Version vs []

-- rvers :: [(PackageName, Maybe Version)] -> ResultVersions
-- rvers = resultVersionsFromList
-- rvers = undefined

simpleResultBody :: PackageName -> [Int] -> ResultBody
simpleResultBody name vs = SimpleResultBody name mversion where
  mversion = case vs of
    [] -> Nothing
    _ -> Just $ ver vs

