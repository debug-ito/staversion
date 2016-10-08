-- |
-- Module: Staversion.Internal.Format
-- Description: formatting Result output.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Format
       ( Formatter,
         defaultFormatter,
         formatResults
       ) where

import Data.Function (on)
import Data.List (groupBy)
import Data.Monoid (mempty, mconcat, (<>))
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, toLazyText, fromText, fromString)
import Data.Version (showVersion)

import Staversion.Internal.Query
  ( Result(..), Query(..),
    sourceDesc,
    ResultVersions,
    resultVersionsToList
  )

data Formatter = Formatter

defaultFormatter :: Formatter
defaultFormatter = Formatter

formatResults :: Formatter -> [Result] -> TL.Text
formatResults f@Formatter = toLazyText . mconcat . map (formatGroupedResults f) . groupBy ((==) `on` resultIn)

formatGroupedResults :: Formatter -> [Result] -> Builder
formatGroupedResults _ [] = mempty
formatGroupedResults formatter@Formatter results@(head_ret : _) = header <> (mconcat $ map single_result_output results) where
  header = "-- " <> (fromText $ sourceDesc $ resultIn head_ret) <> "\n"
  single_result_output ret = case resultVersions ret of
    Left _ -> error_result ret
    Right versions -> formatVersions formatter (resultFor ret) versions
  error_result ret = case resultFor ret of
    QueryName query_name -> "---- " <> fromText query_name <> ": ERROR\n"

formatVersions :: Formatter -> Query -> ResultVersions -> Builder
formatVersions Formatter (QueryName _) rvers = mconcat $ map format $ resultVersionsToList rvers where
  format (name, mver) = case mver of
    Nothing -> "---- " <> fromText name <> ": N/A\n"
    Just ver -> fromText name <> ": ==" <> (fromString $ showVersion ver) <> "\n"

