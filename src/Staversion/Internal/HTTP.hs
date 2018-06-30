-- |
-- Module: Staversion.Internal.HTTP
-- Description: compatibility wrapper for http-client 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- Every module in this package should use this module instead of
-- "Network.HTTP.Client".
--
-- __This is an internal module. End-users should not use it.__
{-# LANGUAGE CPP, DeriveDataTypeable #-}
module Staversion.Internal.HTTP
       ( Manager,
         OurHttpException,
         niceHTTPManager,
         fetchURL,
         asStatusFailureException
       ) where

import Control.Applicative ((<$>))
import Control.Exception (throwIO, Exception, SomeException, catch)
import Data.Typeable (Typeable)
import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Client as H
import Network.HTTP.Client (Manager, HttpException)
import Network.HTTP.Types (statusIsSuccessful)
import Network.HTTP.Client.TLS (tlsManagerSettings)

data OurHttpException = ParseUrlException String SomeException
                      | StatusFailureException H.Request (H.Response ())
                      | OtherHttpException H.HttpException
                      deriving (Show,Typeable)

instance Exception OurHttpException

niceHTTPManager :: IO Manager
niceHTTPManager = H.newManager $ H.managerSetProxy (H.proxyEnvironment Nothing) $ tlsManagerSettings

makeRequest :: String -> Either SomeException H.Request
#if MIN_VERSION_http_client(0,4,30)
makeRequest = H.parseRequest
#else
makeRequest = fmap unCheck . H.parseUrl where
  unCheck req = req { H.checkStatus = \_ _ _ -> Nothing }
#endif

fetchURL :: Manager -> String -> IO BSL.ByteString
fetchURL man url = doFetch `catch` rethrower where
  doFetch = do
    req <- either (\err -> throwIO $ ParseUrlException url err) return $ makeRequest url
    res <- H.httpLbs req man
    checkResponseStatus res req
    return $ H.responseBody res
  checkResponseStatus res req =
    if not $ statusIsSuccessful $ H.responseStatus res
    then throwIO $ StatusFailureException req (const () <$> res)
    else return ()
  rethrower :: H.HttpException -> IO a
  rethrower e = throwIO $ OtherHttpException e

asStatusFailureException :: OurHttpException
                         -> Maybe Int -- ^ HTTP status code
asStatusFailureException (StatusFailureException _ res) = Just code
  where
    code = fromEnum $ H.responseStatus res
asStatusFailureException _ = Nothing
