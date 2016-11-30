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
import Data.List (intersperse)
import Data.Monoid (mempty, mconcat, (<>))
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, toLazyText, fromText, fromString)
import Data.Version (showVersion)

import Staversion.Internal.Query
  ( Result(..), Query(..),
    sourceDesc,
    ResultBody(..)
  )

-- | format 'Result's like it's in build-depends in .cabal files.
formatResultsCabal :: [Result] -> TL.Text
formatResultsCabal = toLazyText . mconcat . map formatGroupedResultsCabal . groupAllPreservingOrderBy ((==) `on` resultIn)

groupAllPreservingOrderBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupAllPreservingOrderBy sameGroup = map snd  . foldr f [] where
  f item acc = update [] acc where
    update heads [] = (item, [item]) : heads
    update heads (cur@(cur_item, cur_list) : rest) =
      if sameGroup item cur_item
      then heads ++ ( (cur_item, item : cur_list) : rest )
      else update (heads ++ [cur]) rest

formatGroupedResultsCabal :: [Result] -> Builder
formatGroupedResultsCabal [] = mempty
formatGroupedResultsCabal results@(head_ret : _) = header <> (concatLines $ single_result_output =<< results) where
  header = "------ " <> (fromText $ sourceDesc $ resultIn head_ret) <> header_real_source <> "\n"
  header_real_source = maybe "" fromText $ resultReallyIn head_ret >>= \real_source -> do
    return (" (" <> sourceDesc real_source <> ")")
  single_result_output ret = case resultBody ret of
    Left _ -> [Left $ error_result ret]
    Right ret_body -> formatVersionsCabal (resultFor ret) ret_body
  error_result ret = case resultFor ret of
    QueryName query_name -> "-- " <> fromText query_name <> " ERROR"
  concatLines ebuilder_lines = (mconcat $ intersperse "\n" $ map (either id id) $ tailCommas ebuilder_lines) <> "\n\n"
  tailCommas = fst . foldr f ([], False) where
    -- flag: True if it has already encountered the last Right element in the list.
    f eb (ret, flag) = let (next_e, next_flag) = getNext ret flag eb
                       in (next_e:ret, next_flag)
    getNext [] flag e@(Left _) = (e, flag)
    getNext _ flag (Left b) = (Left (b <> ","), flag)
    getNext _ False e@(Right _) = (e, True)
    getNext _ True (Right b) = (Right (b <> ","), True)

formatVersionsCabal :: Query -> ResultBody -> [Either Builder Builder]
formatVersionsCabal _ (SimpleResultBody name mver) = [formatted] where
  formatted = case mver of
    Nothing -> Left $ "-- " <> fromText name <> " N/A"
    Just ver -> Right $ fromText name <> " ==" <> (fromString $ showVersion ver)

