module Snap.Snaplet.CouchDb.Http where

import Snap.Snaplet.CouchDb.Http.Cookies (freshenCookies)
import Snap.Snaplet.CouchDb.Http.Types (RequestBody (RequestBodyProducer), reqBody)
import Snap.Snaplet.CouchDb.Utils (liftedNow)

-- httpRaw req sock httpApi t =
--     let (req', coks) = freshenCookies t req httpApi

b req = case reqBody req of
    RequestBodyProducer cl prod -> (Just cl, respond prod) 
