{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Snap.Snaplet.CouchDb where

import Control.Exception (IOException)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Proxy (Producer)
import Data.Default (Default (def))
import Data.Int (Int64)
import Data.Time (UTCTime)
import Data.Word (Word64)

import qualified Blaze.ByteString.Builder as BB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as H
import qualified Network.Socket as N

mkPath :: [Path] -> Path
mkPath = BB.toByteString . H.encodePathSegments .
         map T.decodeUtf8 . filter (/= "")

withCouch :: N.Socket -> Connection -> ((N.Socket, Connection) -> m a) -> m a
withCouch s c f = f (s, c)

mkCouchRequest
    :: H.Method
    -> (Path -> Path)
    -> Connection
    -> H.RequestHeaders
    -> H.Query
    -> RequestBody p m
    -> Request p m
mkCouchRequest meth pathFn cxtn hdrs qrys body = def {
    reqMethod   = meth,
    reqHost     = connHost cxtn,
    reqHeaders  = hdrs,
    reqPort     = connPort cxtn,
    reqPath     = pathFn $ connPref cxtn,
    reqQuery    = H.renderQuery False qrys,
    reqBody     = body
}

type Path = B.ByteString

type Revision = B.ByteString

type CouchResponse p m = Response (Producer p B.ByteString m ())

class (Monad m) => MonadCouch m where
    conn :: m (N.Socket, Connection)

instance (Monad m) => MonadCouch (ReaderT (N.Socket, Connection) m) where
    conn = ask

data Connection = Connection {
    connHost :: B.ByteString,
    connPort :: Int,
    connUser :: B.ByteString,
    connPass :: B.ByteString,
    connPref :: B.ByteString
}
  deriving (Show)

instance Default Connection where
    def = Connection "localhost" 5984 B.empty B.empty B.empty

data CouchError =
    CouchHttpError Int B.ByteString
  | CouchInternalError B.ByteString
  | NotModified
  deriving (Show)

data Response bod = Response {
    resStatus       :: H.Status,
    resHttpVersion  :: H.HttpVersion,
    resHeaders      :: H.ResponseHeaders,
    resBody         :: bod,
    resCookies      :: Cookies
}
  deriving (Show)

newtype Cookies = CS {
    expose :: [Cookie]
}
  deriving (Show)

instance Default Cookies where
    def = CS []

data Cookie = Cookie {
    name            :: B.ByteString,
    value           :: B.ByteString,
    expiryTime      :: UTCTime,
    domain          :: B.ByteString,
    path            :: B.ByteString,
    creationTime    :: UTCTime,
    lastAccessTime  :: UTCTime,
    persistentFlag  :: Bool,
    hostOnlyFlag    :: Bool,
    secureOnlyFlag  :: Bool,
    httpOnlyFlag    :: Bool
}
  deriving (Show)

data Request p m = Request {
    reqMethod           :: H.Method,
    reqSecure           :: Bool,
    reqHost             :: B.ByteString,
    reqPort             :: Int,
    reqPath             :: B.ByteString,
    reqQuery            :: B.ByteString,
    reqHeaders          :: H.RequestHeaders,
    reqBody             :: RequestBody p m,
    reqRedirectCount    :: Int,
    reqCookies          :: Maybe Cookies
}

data RequestBody p m =
    RequestBodyLazy L.ByteString
  | RequestBody B.ByteString
  | RequestBodyBuilder Int64 BB.Builder
  | RequestBodyProducer Int64 (Producer p BB.Builder m ())
  | RequestBodySourceChunked (Producer p BB.Builder m ())

instance Default (Request p m) where
    def = Request {
        reqMethod           = "GET",
        reqSecure           = False,
        reqHost             = "localhost",
        reqPort             = 80,
        reqPath             = "/",
        reqQuery            = B.empty,
        reqHeaders          = [],
        reqBody             = RequestBodyLazy L.empty,
        reqRedirectCount    = 10,
        reqCookies          = Just def
    }


data Proxy = Proxy {
    proxyHost   :: B.ByteString,
    proxyPort   :: Int
}
  deriving (Show)

data HttpError =
    Status H.Status H.ResponseHeaders Cookies
  | InvalidUrl String String
  | MaxRedirects [Response L.ByteString]
  | UnparseableRedirect (Response L.ByteString)
  | MaxRetries
  | HttpParse String
  | HandshakeFailure
  | MaxLengthHeaders
  | ResponseTimeout
  | FailedConnection String Int
  | ExpectedBlankAfter100Continue
  | InvalidStatusLine B.ByteString
  | InvalidHeader B.ByteString
  | InternalIO IOException
  | FailedProxyConnection B.ByteString Int (Either B.ByteString HttpError)
  | NoResponseData
  | TlsError
  | ResponseBodyLength Word64 Word64
  | InvalidChunkHeaders
  deriving (Show)
