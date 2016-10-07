-- |
-- Module: Staversion.Internal.Log
-- Description: types and functions about logging
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Staversion.Internal.Log
       ( LogLevel(..),
         defaultLogger,
         putLog
       ) where

import Control.Monad (when)
import System.IO (Handle, stderr, hPutStrLn)

data LogLevel = LogDebug
              | LogInfo
              | LogWarn
              | LogError
              deriving (Show,Eq,Ord,Enum,Bounded)

data Logger = Logger { loggerThreshold :: LogLevel,
                       loggerHandle :: Handle
                     }

defaultLogger :: Logger
defaultLogger = Logger { loggerThreshold = LogInfo,
                         loggerHandle = stderr
                       }

toLabel :: LogLevel -> String
toLabel l = case l of
  LogDebug -> "[debug]"
  LogInfo ->  "[info]"
  LogWarn ->  "[warn]"
  LogError -> "[error]"

putLog :: Logger -> LogLevel -> String -> IO ()
putLog logger level raw_msg = when (level >= threshold) $ hPutStrLn log_handle msg where
  threshold = loggerThreshold logger
  log_handle = loggerHandle logger
  msg = toLabel level ++ " " ++ raw_msg
