-- |
-- Module: Staversion.Internal.HTTP
-- Description: compatibility wrapper for http-client 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
{-# LANGUAGE CPP #-}
module Staversion.Internal.HTTP
       ( Manager,
         HttpException,
         niceHTTPManager,
         fetchURL
       ) where

import Control.Exception (throwIO)
import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Client as H
import Network.HTTP.Client (Manager, HttpException)
import Network.HTTP.Types (statusIsSuccessful)
import Network.HTTP.Client.TLS (tlsManagerSettings)


niceHTTPManager :: IO Manager
niceHTTPManager = H.newManager $ H.managerSetProxy (H.proxyEnvironment Nothing) $ tlsManagerSettings

makeRequest :: String -> IO H.Request
#if MIN_VERSION_http_client(0,4,30)
makeRequest = H.parseRequest
#else
makeRequest = fmap unCheck . H.parseUrl where
  unCheck req = req { H.checkStatus = \_ _ _ -> Nothing }
#endif

fetchURL :: Manager -> String -> IO BSL.ByteString
fetchURL man url = do
  req <- makeRequest url
  res <- H.httpLbs req man
  checkResponseStatus res
  return $ H.responseBody res
  where
    checkResponseStatus res =
      if not $ statusIsSuccessful $ res_status
      then throwIO $ H.StatusCodeException res_status res_headers cookie_jar
      else return ()
      where
        res_status = H.responseStatus res
        res_headers = H.responseHeaders res
        cookie_jar = H.responseCookieJar res
