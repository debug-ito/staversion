-- |
-- Module: Staversion.Internal.EIO
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.EIO
       ( EIO,
         runEIO,
         toEIO,
         toEIOShow,
         loggedElse,
         maybeToEIO,
         eitherToEIO
       ) where

import Control.Monad.Trans.Except (runExceptT, ExceptT(..))
import qualified Data.Bifunctor as Bi

import Staversion.Internal.Log (Logger, logWarn)
import Staversion.Internal.Query (ErrorMsg)

type EIO = ExceptT ErrorMsg IO

runEIO :: EIO a -> IO (Either ErrorMsg a)
runEIO = runExceptT

toEIO :: IO (Either ErrorMsg a) -> EIO a
toEIO = ExceptT

toEIOShow :: Show e => IO (Either e a) -> EIO a
toEIOShow = toEIO . fmap (Bi.first show)

loggedElse :: Logger
           -> EIO a -- ^ first action tried.
           -> EIO a -- ^ the action executed if the first action returns 'Left'.
           -> EIO a
loggedElse logger first second = ExceptT $ do
  eret <- runExceptT first
  case eret of
   Right _ -> return eret
   Left e -> logWarn logger e >> runExceptT second

maybeToEIO :: ErrorMsg -> Maybe a -> EIO a
maybeToEIO msg = ExceptT . return . maybe (Left msg) Right

eitherToEIO :: Either ErrorMsg a -> EIO a
eitherToEIO = ExceptT . return


