module Snap.Snaplet.CouchDb.Http where

import Snap.Snaplet.CouchDb.Http.Cookies (addCookies, rmStaleCookies)
import Snap.Snaplet.CouchDb.Utils

freshenCookies now req coks = addCookies req (rmStaleCookies now coks) now
