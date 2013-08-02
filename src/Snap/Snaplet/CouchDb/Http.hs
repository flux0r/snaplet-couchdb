module Snap.Snaplet.CouchDb.Http where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (UTCTime, getCurrentTime)

import Snap.Snaplet.CouchDb.Http.Cookies (addCookies, rmExpiredCookies)

liftedNow :: MonadIO m => m UTCTime
liftedNow = liftIO getCurrentTime

freshenCookies now req coks = addCookies req (rmExpiredCookies coks now) now
