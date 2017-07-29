-- |
-- Module: Staversion.Internal.Megaparsec
-- Description: Megaparsec compatibility wrapper
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Staversion.Internal.Megaparsec
       ( module Text.Megaparsec,
         -- module Text.Megaparsec.Text

         module Text.Megaparsec.Char,
         Parser,
         string,
         string'
       ) where

import Text.Megaparsec
-- import Text.Megaparsec.Text

import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec.Char hiding (string, string')
import qualified Text.Megaparsec.Char as MC

type Parser = Parsec Void Text

liftToString :: Monad m => (Text -> m Text) -> String -> m String
liftToString f = fmap unpack . f . pack

-- | 'string' combatible with Megaparsec 5. In Megaparsec 6, 'string'
-- function takes and produces 'Text' if we use 'Text' 'Stream',
-- probably for better performance.
string :: String -> Parser String
string = liftToString MC.string

-- | 'string'' combatible with Megaparsec 5.
string' :: String -> Parser String
string' = liftToString MC.string'
