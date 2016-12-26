module Staversion.Internal.TestUtil
       ( ver, simpleResultBody,
         verPairs
       ) where

import Data.Version (Version(..))
import Staversion.Internal.Query ( PackageName
                                 )
import Staversion.Internal.Result (ResultBody(..))

ver :: [Int] -> Version
ver vs = Version vs []

verMaybe :: [Int] -> Maybe Version
verMaybe [] = Nothing
verMaybe vs = Just $ ver vs

verPairs :: [(PackageName, [Int])] -> [(PackageName, Maybe Version)]
verPairs = map f where
  f (pname, vs) = (pname, verMaybe vs)

simpleResultBody :: PackageName -> [Int] -> ResultBody
simpleResultBody name vs = SimpleResultBody name $ verMaybe vs
