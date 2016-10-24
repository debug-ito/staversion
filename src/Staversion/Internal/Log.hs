-- |
-- Module: Staversion.Internal.Log
-- Description: types and functions about logging
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Staversion.Internal.Log
       ( LogLevel(..),
         Logger(loggerThreshold),
         defaultLogger,
         putLog,
         logDebug,
         logInfo,
         logWarn
       ) where

import Control.Monad (when)
import System.IO (Handle, stderr, hPutStrLn)

data LogLevel = LogDebug
              | LogInfo
              | LogWarn
              | LogError
              deriving (Show,Eq,Ord,Enum,Bounded)

data Logger = Logger { loggerThreshold :: Maybe LogLevel,
                       -- ^ If 'Nothing', logging is disabled.
                       loggerPutString :: String -> IO ()
                     }

instance Show Logger where
  show l = "Logger { loggerThreshold = " ++ show (loggerThreshold l) ++ " }"

defaultLogger :: Logger
defaultLogger = Logger { loggerThreshold = Just LogInfo,
                         loggerPutString = hPutStrLn stderr
                       }

toLabel :: LogLevel -> String
toLabel l = case l of
  LogDebug -> "[debug]"
  LogInfo ->  "[info]"
  LogWarn ->  "[warn]"
  LogError -> "[error]"

putLog :: Logger -> LogLevel -> String -> IO ()
putLog logger level raw_msg = when (fmap (level >=) mthreshold == Just True) $ loggerPutString logger msg where
  mthreshold = loggerThreshold logger
  msg = toLabel level ++ " " ++ raw_msg

logDebug :: Logger -> String -> IO ()
logDebug = flip putLog $ LogDebug

logInfo :: Logger -> String -> IO ()
logInfo = flip putLog $ LogInfo

logWarn :: Logger -> String -> IO ()
logWarn = flip putLog $ LogWarn

