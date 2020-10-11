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
         textSatisfying,

#if MIN_VERSION_megaparsec(7,0,0)
         anyChar,
#endif

#if MIN_VERSION_megaparsec(6,0,0)
         module Text.Megaparsec.Char,
         string,
         string'
#else
         space1
#endif
       ) where

import Control.Applicative (many, some)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec

-----------------------------------------------------

#if MIN_VERSION_megaparsec(6,0,0)
import Data.Text (pack, unpack)
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

#elif MIN_VERSION_megaparsec(5,0,0)

-- | From "Text.Megaparsec.Text".
type Parser = Parsec Dec Text

#else

-- | From "Text.Megaparsec.Text"
type Parser = Parsec Text

#endif

-----------------------------------------------------

#if MIN_VERSION_megaparsec(7,0,0)

anyChar :: Parser Char
anyChar = anySingle

#endif

-----------------------------------------------------

textSatisfying :: (Char -> Bool) -> Parser Text
#if MIN_VERSION_megaparsec(6,0,0)
textSatisfying p = takeWhileP Nothing p
#else
textSatisfying p = fmap T.pack $ many $ satisfy p
#endif

#if MIN_VERSION_megaparsec(6,0,0)
#else
space1 :: Parser ()
space1 = skipSome spaceChar
#endif
