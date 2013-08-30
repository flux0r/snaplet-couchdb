{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.CouchDb.Http.Request where

import Data.Int (Int64)
import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive (CI)
import Data.Monoid (mappend, mempty)
import qualified Blaze.ByteString.Builder as BB
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Control.Error (note, readErr)
import Snap.Snaplet.CouchDb.Http.Types (HttpError (InvalidUrl, StatusError),
                                        Request (..), RequestBody (..),
                                        ContentType, Cookies)
import qualified Network.URI as U
import qualified Network.HTTP.Types as H
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map


------------------------------------------------------------------------------
-- Constants

defaultReqRedirectMax,
    defaultTimeout,
    defaultPort,
    defaultSecPort :: Int
defaultReqRedirectMax       = 10
defaultTimeout              = 5
defaultPort                 = 80
defaultSecPort              = 443

schemeHttp,
    schemeHttps :: String
schemeHttp = "http:"
schemeHttps = "https:"

contentLengthHeaderKey :: CI ByteString
contentLengthHeaderKey = "Content-Length"

tarBall :: ContentType
tarBall = "application/x-tar"


------------------------------------------------------------------------------
-- URIs

escape :: String -> String
escape = U.escapeURIString U.isAllowedInURI

parseUrl :: String -> Either HttpError U.URI
parseUrl x = note (InvalidUrl x msg) (U.parseURI $ escape x)
  where
    msg = "Parse failure"

mkUri :: Request p m a -> U.URI
mkUri req = U.URI
    { U.uriScheme       = if reqSecure req then schemeHttps else schemeHttp
    , U.uriAuthority    = Just $ mkUriAuth req
    , U.uriPath         = C8.unpack $ reqPath req
    , U.uriQuery        = C8.unpack $ reqQueryString req
    , U.uriFragment     = maybe "" C8.unpack (reqFrag req)
    }

setUri :: Request p m a -> U.URI -> Either HttpError (Request p m a)
setUri req u =
    let secure = reqSecure req
        path = if null $ U.uriPath u
                   then C8.singleton '/'
                   else C8.pack $ U.uriPath u
    in do
        auth <- getAuth u
        port <- parsePort secure auth (show u)
        scheme <- parseScheme u
        return req
            { reqHost           = C8.pack $ U.uriRegName auth
            , reqPort           = port
            , reqSecure         = secure
            , reqPath           = path
            , reqQueryString    = C8.pack $ U.uriQuery u
            }

mkUriAuth :: Request p m a -> U.URIAuth
mkUriAuth req = U.URIAuth user regname port
  where
    user    = ""
    regname = C8.unpack $ reqHost req
    port    = ":" ++ (show $ reqPort req)

parseScheme :: U.URI -> Either HttpError String
parseScheme u = let s = U.uriScheme u in check s
  where
    check x
      | x == schemeHttp     = Right x
      | x == schemeHttps    = Right x
      | otherwise           = Left $ InvalidUrl (show u) msg
    msg = "Invalid scheme"

parsePort :: Bool -> U.URIAuth -> String -> Either HttpError Int
parsePort secure auth msg =
    case U.uriPort auth of
        ':':rest   -> readErr (invalid "Invalid Port") rest
        ""         -> Right fallback
        _          -> Left $ invalid "Invalid Port"
  where
    invalid = InvalidUrl msg
    fallback = if secure then defaultSecPort else defaultPort

getAuth :: U.URI -> Either HttpError U.URIAuth
getAuth u = note (InvalidUrl (show u) "Relative URLs not supported")
                 (U.uriAuthority u)


------------------------------------------------------------------------------
-- Requests

browserDecompress :: ContentType -> Bool
browserDecompress = (/=) tarBall

decompress :: ContentType -> Bool
decompress = const True

defaultRequest :: Request p m a
defaultRequest = Request
    { reqMethod             = H.GET
    , reqHost               = "localhost"
    , reqPort               = 80
    , reqPath               = C8.singleton '/'
    , reqQueryString        = C8.empty
    , reqHeaders            = Map.empty
    , reqBody               = RequestBodyLazy L.empty
    , reqHostAddr           = Nothing
    , reqRawBody            = False
    , reqDecompress         = browserDecompress
    , reqRedirectMax        = defaultReqRedirectMax
    , reqCheckStatus        = defaultReqCheckStatus
    , reqTimeout            = Just defaultTimeout
    , reqCookies            = Map.empty
    , reqSecure             = False
    , reqFrag               = Nothing
    }

instance Show (Request p m a) where
    show x = unlines
        [ "Request {"
        , "\tmethod             = " ++ show (reqMethod x)
        , "\thost               = " ++ show (reqHost x)
        , "\tport               = " ++ show (reqPort x)
        , "\tpath               = " ++ show (reqPath x)
        , "\tquery              = " ++ show (reqQueryString x)
        , "\theaders            = " ++ show (reqHeaders x)
        , "\thost address       = " ++ show (reqHostAddr x)
        , "\traw body           = " ++ show (reqRawBody x)
        , "\ttimeout            = " ++ show (reqTimeout x)
        , "\tcookies            = " ++ show (reqCookies x)
        , "\tsecure             = " ++ show (reqSecure x)
        , "\tfragment           = " ++ show (reqFrag x)
        ]


------------------------------------------------------------------------------
-- Status

normalStatus :: Int -> Bool
normalStatus code = code >= 200 && code < 300

defaultReqCheckStatus :: H.Status
                      -> H.ResponseHeaders
                      -> Cookies
                      -> Maybe HttpError
defaultReqCheckStatus s@(H.Status code _) hs coks =
    if normalStatus code
        then Nothing
        else Just $ StatusError s hs coks


------------------------------------------------------------------------------
-- Headers

hostHeader :: Request p m a -> ByteString
hostHeader req =
    let hs = reqHeaders req
        v = hostHeaderValue req
    in  Map.findWithDefault v hostHeaderKey hs

hostHeaderKey :: CI ByteString
hostHeaderKey = "Host"
 
hostHeaderValue :: Request p m a -> ByteString
hostHeaderValue req
    | standard      = host
    | otherwise     = host `mappend` (C8.pack . show $ port)
  where
    port = reqPort req
    secure = reqSecure req
    host = reqHost req
    standard =
            port == defaultPort && not secure
        ||  port == defaultSecPort && secure

contentLength :: RequestBody p m a -> Maybe Int64
contentLength (RequestBodyLazy b)       = Just . L.length $ b
contentLength (RequestBody b)           = Just . fromIntegral . B.length $ b
contentLength (RequestBodyProducer i _) = Just i
contentLength _                         = Nothing


------------------------------------------------------------------------------
-- Builders

contentLengthBuilder :: (Eq a, Num a, Show a)
                     => Maybe a 
                     -> H.StdMethod
                     -> BB.Builder
contentLengthBuilder (Just cl) method = case (cl, method) of
    (0, H.GET)      -> mempty
    (0, H.HEAD)     -> mempty
    _               -> headerToBuilder contentLengthHeaderKey clStr
  where
    clStr = C8.pack . show $ cl

headerToBuilder :: H.HeaderName -> ByteString -> BB.Builder
headerToBuilder k v = BB.fromByteString (CI.original k)
            `mappend` BB.fromByteString ": "
            `mappend` BB.fromByteString v
            `mappend` BB.fromByteString "\r\n"
