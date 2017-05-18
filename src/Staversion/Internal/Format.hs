-- |
-- Module: Staversion.Internal.Format
-- Description: formatting Result output.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Format
       ( formatResults,
         FormatConfig(..),
         FormatVersion
       ) where

import Data.Foldable (fold)
import Data.Function (on)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NL
import Data.Monoid (mempty, mconcat, (<>))
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, toLazyText, fromText, fromString)
import Distribution.Version (VersionRange)

import Staversion.Internal.Aggregate
  ( Aggregator, showVersionRange, groupAllPreservingOrderBy,
    aggregateResults
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

-- | Format for 'VersionRange'.
type FormatVersion = VersionRange -> Text

data FormatConfig = FormatConfig { fconfAggregator :: Maybe Aggregator,
                                   fconfFormatVersion :: FormatVersion
                                 }



-- | Format 'Result's with the given 'FormatConfig'.
formatResults :: FormatConfig -> [Result] -> (TL.Text, [LogEntry])
formatResults fconf = (\(aggs, logs) -> (formatAggregatedResults fconf aggs, logs))
                      . aggregateResultsWithConf
  where
    aggregateResultsWithConf = case fconfAggregator fconf of
      Nothing -> \results -> (map singletonResult results, [])
      Just agg -> aggregateResults agg



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
    ((QueryName name), Left _) -> (blocks, (packageErrorLine name) : rlines)
    ((QueryCabalFile file), Left _) -> (cabalFileErrorBlock file : prependLines blocks rlines, [])

versionLine :: FormatVersion -> PackageName -> Maybe VersionRange -> ResultLine
versionLine _ name Nothing = Left $ "-- " <> fromText name <> " N/A"
versionLine format_version name (Just ver_range) = Right $ fromText name <> " " <> (fromText $ format_version ver_range)

packageErrorLine :: PackageName -> ResultLine
packageErrorLine name = Left $ "-- " <> fromText name <> " ERROR"

cabalFileErrorBlock :: FilePath -> ResultBlock
cabalFileErrorBlock file = RBLines [Left line] where
  line = "-- " <> fromString file <> " ERROR"

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
