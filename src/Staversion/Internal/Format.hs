-- |
-- Module: Staversion.Internal.Format
-- Description: formatting Result output.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Format
       ( formatResultsCabal
       ) where

import Data.Function (on)
import Data.List (groupBy, intersperse)
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

-- | format 'Result's like it's in build-depends in .cabal files.
formatResultsCabal :: [Result] -> TL.Text
formatResultsCabal = toLazyText . mconcat . map formatGroupedResultsCabal . groupBy ((==) `on` resultIn)

formatGroupedResultsCabal :: [Result] -> Builder
formatGroupedResultsCabal [] = mempty
formatGroupedResultsCabal results@(head_ret : _) = header <> (concatLines $ map single_result_output results) where
  header = "------ " <> (fromText $ sourceDesc $ resultIn head_ret) <> "\n"
  single_result_output ret = case resultVersions ret of
    Left _ -> error_result ret
    Right versions -> formatVersionsCabal (resultFor ret) versions
  error_result ret = case resultFor ret of
    QueryName query_name -> "-- " <> fromText query_name <> " ERROR"
  concatLines builder_lines = (mconcat $ intersperse ",\n" builder_lines) <> "\n\n"

formatVersionsCabal :: Query -> ResultVersions -> Builder
formatVersionsCabal (QueryName _) rvers = mconcat $ map format $ resultVersionsToList rvers where
  format (name, mver) = case mver of
    Nothing -> "-- " <> fromText name <> " N/A"
    Just ver -> fromText name <> " ==" <> (fromString $ showVersion ver)

