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
import Data.Version (showVersion, Version)

import Staversion.Internal.Query
  ( Query(..),
    sourceDesc,
    PackageName
  )
import Staversion.Internal.Result (Result(..), ResultBody(..))

-- | format 'Result's like it's in build-depends in .cabal files.
formatResultsCabal :: [Result] -> TL.Text
formatResultsCabal = toLazyText . mconcat . map formatResultBlock . makeSourceBlocks

groupAllPreservingOrderBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupAllPreservingOrderBy sameGroup = map snd  . foldr f [] where
  f item acc = update [] acc where
    update heads [] = (item, [item]) : heads
    update heads (cur@(cur_item, cur_list) : rest) =
      if sameGroup item cur_item
      then heads ++ ( (cur_item, item : cur_list) : rest )
      else update (heads ++ [cur]) rest

-- | 'Left' lines and 'Right' lines are handled differently by
-- 'formatResultBlock'. It puts commas at the right places assuming
-- 'Left' lines are commented out.
type ResultLine = Either Builder Builder

data ResultBlock = RBHead Builder [ResultBlock] -- ^ header and child blocks
                 | RBLines [ResultLine] -- ^ a block, which consists of some lines.

makeSourceBlocks :: [Result] -> [ResultBlock]
makeSourceBlocks = map sourceBlock . groupAllPreservingOrderBy ((==) `on` resultIn) where
  sourceBlock [] = RBLines []
  sourceBlock results@(head_ret : _) = RBHead header $ makeQueryBlocks results where
    header = "------ " <> (fromText $ sourceDesc $ resultIn head_ret) <> header_real_source
    header_real_source = maybe "" fromText $ resultReallyIn head_ret >>= \real_source -> do
      return (" (" <> sourceDesc real_source <> ")")

makeQueryBlocks :: [Result] -> [ResultBlock]
makeQueryBlocks = uncurry prependLines . foldr f ([], []) where
  prependLines blocks [] = blocks
  prependLines blocks rlines = (RBLines rlines) : blocks
  f ret (blocks, rlines) = case (resultFor ret, resultBody ret) of
    (_, Right (SimpleResultBody name mver)) -> (blocks, (versionLine name mver) : rlines)
    (_, Right (CabalResultBody _ _ _)) -> undefined -- TODO
    ((QueryName name), Left _) -> (blocks, (packageErrorLine name) : rlines)
    ((QueryCabalFile file), Left _) -> (cabalFileErrorBlock file : prependLines blocks rlines, [])

versionLine :: PackageName -> Maybe Version -> ResultLine
versionLine name Nothing = Left $ "-- " <> fromText name <> " N/A"
versionLine name (Just ver) = Right $ fromText name <> " ==" <> (fromString $ showVersion ver)

packageErrorLine :: PackageName -> ResultLine
packageErrorLine name = Left $ "-- " <> fromText name <> " ERROR"

cabalFileErrorBlock :: FilePath -> ResultBlock
cabalFileErrorBlock file = RBLines [Left line] where
  line = "-- " <> fromString file <> " ERROR"

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
