{-# LANGUAGE RankNTypes #-}

module Snap.Snaplet.CouchDb.Http.Types where

import Data.Int (Int64)
import Data.Word (Word64)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Types as H
import qualified Network.Socket as N
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time
import Blaze.ByteString.Builder
import Control.Proxy
import Control.Exception (IOException)

type ContentType = ByteString
type ReqCheckStatus = H.Status -> H.ResponseHeaders ->
                            Cookies -> Maybe HttpError
type Headers = Map H.HeaderName ByteString
type RequestHeaders = Headers
type ResponseHeaders = Headers
type CookieKey = (ByteString, ByteString, ByteString)
type Cookies = Map CookieKey Cookie

data Request p m a = Request {
    reqMethod       :: H.Method,
    reqHost         :: ByteString,
    reqPort         :: Int,
    reqPath         :: ByteString,
    reqQueryString  :: ByteString,
    reqHeaders      :: RequestHeaders,
    reqBody         :: RequestBody p m a,
    reqHostAddr     :: (Maybe N.HostAddress),
    reqRawBody      :: Bool,
    reqDecompress   :: ContentType -> Bool,
    reqRedirectMax  :: Int,
    reqCheckStatus  :: ReqCheckStatus,
    reqTimeout      :: (Maybe Int),
    reqCookies      :: Cookies,
    reqSecure       :: Bool,
    reqFrag         :: Maybe ByteString
}

data Cookie = Cookie {
    cookieName              :: ByteString,
    cookieValue             :: ByteString,
    cookieExpires           :: (Maybe UTCTime),
    cookieDomain            :: (Maybe ByteString),
    cookiePath              :: (Maybe ByteString),
    cookieCreated           :: (Maybe UTCTime),
    cookieLastAccessed      :: (Maybe UTCTime),
    cookiePersistentFlag    :: Bool,
    cookieHostOnlyFlag      :: Bool,
    cookieSecureOnlyFlag    :: Bool,
    cookieHttpOnlyFlag      :: Bool
}
  deriving (Show)

data RequestBody p m a =
    RequestBodyLazy L.ByteString
  | RequestBody ByteString
  | RequestBodyBuilder Int64 Builder
  | RequestBodyProducer Int64 (Producer p Builder m a)
  | RequestBodyChunked (Producer p Builder m a)

data HttpProxy = Proxy {
    proxyHost :: ByteString,
    proxyPort :: Int
}

data HttpError =
    StatusError H.Status H.ResponseHeaders Cookies
  | InvalidUrl String String
  | TooManyRedirects [Response L.ByteString]
  | UnparseableRedirect (Response L.ByteString)
  | TooManyRetries
  | UnparseableHttp String
  | HandshakeFailure
  | TooLongHeaders
  | ResponseTimeout
  | ConnectionFailure String Int
  | ExpectedBlankAfter100Continue
  | InvalidStatusLine ByteString
  | InvalidHeader ByteString
  | InternalIOError IOException
  | ProxyConnectError ByteString Int (Either ByteString HttpError)
  | NoResponseData
  | TooShortResponseBody Word64 Word64
  | InvalidChunkHeaders

data Response body = Response {
    resStatus       :: H.Status,
    resVersion      :: H.HttpVersion,
    resHeaders      :: ResponseHeaders,
    resBody         :: body,
    resCookies      :: Cookies
}

instance Functor Response where
    fmap f res = res { resBody = f (resBody res) }
