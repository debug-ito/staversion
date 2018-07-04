-- |
-- Module: Staversion.Internal.Format
-- Description: formatting Result output.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Format
       ( formatAggregatedResults,
         FormatConfig(..),
         FormatVersion,
         formatVersionCabal,
         formatVersionCabalCaret
       ) where

import Data.Foldable (fold)
import Data.Function (on)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NL
import Data.Maybe (fromJust)
import Data.Monoid (mempty, mconcat, (<>))
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, toLazyText, fromText, fromString)

import Staversion.Internal.Aggregate
  ( groupAllPreservingOrderBy,
    showVersionRange
  )
import Staversion.Internal.Query
  ( Query(..),
    sourceDesc,
    PackageName
  )
import Staversion.Internal.Result
  ( Result(..), ResultBody'(..), ResultSource(..), resultSourceDesc,
    AggregatedResult(..), singletonResult
  )
import Staversion.Internal.Cabal (Target(..))
import Staversion.Internal.Log (LogEntry)
import Staversion.Internal.Version (VersionRange)
import qualified Staversion.Internal.Version as V


-- | Format for 'VersionRange'.
type FormatVersion = VersionRange -> Text

-- | Let Cabal format 'VersionRange'.
formatVersionCabal :: FormatVersion
formatVersionCabal = pack . showVersionRange

-- | Similar to 'formatVersionCabal', but it uses the \"caret\"
-- operator (@^>=@) where possible.
formatVersionCabalCaret :: FormatVersion
formatVersionCabalCaret = Text.intercalate " || " . map formatVersionIntervalCaret . V.asVersionIntervals

formatVersionIntervalCaret :: V.VersionInterval -> Text
formatVersionIntervalCaret vi = case vi of
  (V.LowerBound lv V.InclusiveBound, V.UpperBound uv V.ExclusiveBound) ->
    if isCaretOK lv uv
    then "^>=" <> formatV lv
    else fallback
  _ -> fallback
  where
    formatV v = pack $ concat $ intersperse "." $ map show $ V.versionNumbers v
    fallback = formatVersionCabal $ V.fromVersionIntervals $ V.mkVersionIntervals [vi]

isCaretOK :: V.Version -> V.Version -> Bool
isCaretOK inc_lv exc_uv = isCaretOK' (V.versionNumbers inc_lv) (V.versionNumbers exc_uv) where
  isCaretOK' [] uv'          = uv' == [0,1]
  isCaretOK' [x] uv'         = uv' == [x,1]
  isCaretOK' (x : y : _) uv' = uv' == [x,y+1]



data FormatConfig = FormatConfig { fconfFormatVersion :: FormatVersion
                                 }

-- | 'Left' lines and 'Right' lines are handled differently by
-- 'formatResultBlock'. It puts commas at the right places assuming
-- 'Left' lines are commented out.
type ResultLine = Either Builder Builder

data ResultBlock = RBHead Builder [ResultBlock] -- ^ header and child blocks
                 | RBLines [ResultLine] -- ^ a block, which consists of some lines.


formatAggregatedResults :: FormatConfig -> [AggregatedResult] -> TL.Text
formatAggregatedResults fconf = toLazyText . mconcat . map formatResultBlock . makeSourceBlocks fconf

makeSourceBlocks :: FormatConfig -> [AggregatedResult] -> [ResultBlock]
makeSourceBlocks fconf = map sourceBlock . groupAllPreservingOrderBy ((==) `on` aggResultIn) where
  sourceBlock results@(head_ret :| _) = RBHead header $ makeQueryBlocks fconf $ NL.toList results where
    header = "------ " <> (fold $ NL.intersperse ", " $ fmap sourceHeader $ aggResultIn head_ret)

sourceHeader :: ResultSource -> Builder
sourceHeader = fromText . resultSourceDesc

makeQueryBlocks :: FormatConfig -> [AggregatedResult] -> [ResultBlock]
makeQueryBlocks fconf = uncurry prependLines . foldr f ([], []) where
  prependLines blocks [] = blocks
  prependLines blocks rlines = (RBLines rlines) : blocks
  f ret (blocks, rlines) = case (aggResultFor ret, aggResultBody ret) of
    (_, Right (SimpleResultBody name mver)) -> (blocks, (versionLine (fconfFormatVersion fconf) name mver) : rlines)
    (_, Right (CabalResultBody file target pairs)) -> (cabalFileSuccessBlock fconf file target pairs : prependLines blocks rlines, [])
    (query, Left _) -> case makeQueryErrorReport query of
      Left line -> (blocks, line : rlines)
      Right block -> (block : prependLines blocks rlines, [])

versionLine :: FormatVersion -> PackageName -> Maybe VersionRange -> ResultLine
versionLine _ name Nothing = Left $ "-- " <> fromText name <> " N/A"
versionLine format_version name (Just ver_range) = Right $ fromText name <> " " <> (fromText $ format_version ver_range)

makeQueryErrorReport :: Query -> Either ResultLine ResultBlock
makeQueryErrorReport (QueryName name) = Left $ Left $ "-- " <> fromText name <> " ERROR"
makeQueryErrorReport (QueryCabalFile file) = Right $ errorReportBlock file
makeQueryErrorReport (QueryStackYaml file) = Right $ errorReportBlock file
makeQueryErrorReport QueryStackYamlDefault = Right $ errorReportBlock "default stack.yaml"

errorReportBlock :: String -> ResultBlock
errorReportBlock label = RBLines [Left line] where
  line = "-- " <> fromString label <> " ERROR"

cabalFileSuccessBlock :: FormatConfig -> FilePath -> Target -> [(PackageName, Maybe VersionRange)] -> ResultBlock
cabalFileSuccessBlock fconf file target pairs = RBHead header [RBLines $ map (uncurry $ versionLine $ fconfFormatVersion fconf) pairs] where
  header = "-- " <> fromString file <> " - " <> target_text
  target_text = case target of
    TargetLibrary -> "library"
    TargetExecutable n -> "executable " <> fromText n
    TargetTestSuite n -> "test-suite " <> fromText n
    TargetBenchmark n -> "benchmark " <> fromText n

formatResultBlock :: ResultBlock -> Builder
formatResultBlock (RBHead header blocks) = header <> "\n" <> mconcat (map formatResultBlock blocks)
formatResultBlock (RBLines rlines) = (mconcat $ map ((<> "\n") . either id id) $ tailCommas rlines) <> "\n" where
  tailCommas = fst . foldr f ([], False)
               -- flag: True if it has already encountered the last Right element in the list.
  f eb (ret, flag) = let (next_e, next_flag) = getNext ret flag eb
                     in (next_e:ret, next_flag)
  getNext [] flag e@(Left _) = (e, flag)
  getNext _ flag (Left b) = (Left (b <> ","), flag)
  getNext _ False e@(Right _) = (e, True)
  getNext _ True (Right b) = (Right (b <> ","), True)
