module Snap.Snaplet.CouchDb.Http.Request where

import Control.Error (note)
import Snap.Snaplet.CouchDb.Http.Types (HttpError (InvalidUrl))
import qualified Network.URI as U
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map

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
    }

defaultReqRedirectMax = 10

defaultTimeout = 5

normalStatus code = code >= 200 && code < 300

defaultReqCheckStatus s@(H.Status code _) hs coks =
    if normalStatus code
        then Nothing
        else StatusError code hs coks

tarBall :: ContentType
tarBall = "application/x-tar"

browserDecompress = (/=) tarBall

escape = escapeURIString isAllowedInURI

parseUrl x = note (InvalidUrl x) (U.parseURI $ escape x)
