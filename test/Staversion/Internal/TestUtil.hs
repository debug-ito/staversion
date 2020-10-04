module Staversion.Internal.TestUtil
       ( ver, simpleResultBody,
         verPairs,
         vor,
         vthis,
         vors, vors',
         vint,
         specPackage
       ) where

import Data.Maybe (fromJust)
import Data.Text (unpack)
import Test.Hspec (specify, SpecWith, shouldBe)

import Staversion.Internal.BuildPlan.BuildPlanMap
  ( HasVersions(..)
  )
import Staversion.Internal.Query ( PackageName
                                 )
import Staversion.Internal.Result (ResultBody, ResultBody'(..))
import Staversion.Internal.Version (Version)
import qualified Staversion.Internal.Version as V


ver :: [Int] -> Version
ver = V.mkVersion

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
vint vl vu = V.fromVersionIntervals $ V.mkVersionIntervals [interval] where
  interval = (V.LowerBound (ver vl) V.InclusiveBound, V.UpperBound (ver vu) V.ExclusiveBound)

specPackage :: HasVersions t => PackageName -> Maybe [Int] -> SpecWith t
specPackage pname exp_vers = specify spec_name $ \t -> packageVersion t pname `shouldBe` (fmap V.mkVersion exp_vers)
  where
    spec_name = unpack pname ++ ", " ++ show exp_vers
  
