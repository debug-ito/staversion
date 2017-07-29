{-# LANGUAGE CPP #-}
-- |
-- Module: Staversion.Internal.Megaparsec
-- Description: Megaparsec compatibility wrapper
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Megaparsec
       ( module Text.Megaparsec,
         Parser,

#if MIN_VERSION_megaparsec(6,0,0)
         module Text.Megaparsec.Char,
         string,
         string'
#endif
       ) where

import Text.Megaparsec

#if MIN_VERSION_megaparsec(6,0,0)
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec.Char hiding (string, string')
import qualified Text.Megaparsec.Char as MC

-- | See: https://github.com/fpco/stackage/issues/2666#issuecomment-318472919
type Parser = Parsec (ErrorFancy Void) Text

liftToString :: Monad m => (Text -> m Text) -> String -> m String
liftToString f = fmap unpack . f . pack

-- | 'string' combatible with Megaparsec 5. In Megaparsec 6, 'string'
-- function takes and produces 'Text' if we use 'Text' as the 'Stream'
-- type, probably for better performance.
string :: String -> Parser String
string = liftToString MC.string

-- | 'string'' combatible with Megaparsec 5.
string' :: String -> Parser String
string' = liftToString MC.string'

#else

import Data.Text (Text)

-- | From "Text.Megaparsec.Text".
type Parser = Parsec Dec Text

#endif

