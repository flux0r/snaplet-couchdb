{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.CouchDb.Http.Request where

import Control.Error (note, readErr)
import Snap.Snaplet.CouchDb.Http.Types (HttpError (InvalidUrl, StatusError),
                                        Request (..), RequestBody (..),
                                        ContentType, Cookies)
import qualified Network.URI as U
import qualified Network.HTTP.Types as H
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map

defaultReqRedirectMax,
    defaultTimeout,
    defaultPort,
    defaultSecPort :: Int
defaultReqRedirectMax       = 10
defaultTimeout              = 5
defaultPort                 = 80
defaultSecPort              = 443

schemeHttp = "http:"
schemeHttps = "https:"

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

defaultRequest :: Request p m a
defaultRequest = Request
    { reqMethod             = "GET"
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

tarBall :: ContentType
tarBall = "application/x-tar"

browserDecompress :: ContentType -> Bool
browserDecompress = (/=) tarBall

escape :: String -> String
escape = U.escapeURIString U.isAllowedInURI

parseUrl :: String -> Either HttpError U.URI
parseUrl x = note (InvalidUrl x msg) (U.parseURI $ escape x)
  where
    msg = "Parse failure"
