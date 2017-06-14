module Staversion.Internal.TestUtil
       ( ver, simpleResultBody,
         verPairs,
         vor,
         vthis,
         vors, vors',
         vint
       ) where

import Data.Maybe (fromJust)
import Data.Version (Version(..))
import qualified Distribution.Version as V
import Staversion.Internal.Query ( PackageName
                                 )
import Staversion.Internal.Result (ResultBody, ResultBody'(..))

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

vor :: V.VersionRange -> V.VersionRange -> V.VersionRange
vor = V.unionVersionRanges

vthis :: [Int] -> V.VersionRange
vthis = V.thisVersion . ver

vors :: [[Int]] -> V.VersionRange
vors = vors' . map vthis

vors' :: [V.VersionRange] -> V.VersionRange
vors' [] = error "this should not happen"
vors' [v] = v
vors' (v:rest) = vor v $ vors' rest

-- | Version interval. vint x y = [x, y)
vint :: [Int] -> [Int] ->V.VersionRange
vint vl vu = fromJust $ fmap V.fromVersionIntervals $ V.mkVersionIntervals [interval] where
  interval = (V.LowerBound (ver vl) V.InclusiveBound, V.UpperBound (ver vu) V.ExclusiveBound)

