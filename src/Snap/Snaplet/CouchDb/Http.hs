module Snap.Snaplet.CouchDb.Http where

import Control.Monad.IO.Class (MonadIO)
import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)
import Control.Exception (throwIO)
import Snap.Snaplet.CouchDb.Http.Cookies (freshenCookies)
import Snap.Snaplet.CouchDb.Http.Types (RequestBody (RequestBodyProducer),
                                        Request (..), HttpError)
import Snap.Snaplet.CouchDb.Utils (liftedNow)
import qualified Data.Map as Map
import System.Timeout (timeout)


-- httpNoRedirects req' resource = let cs = reqCookies req' in
--     do
--         (req, cs') <-
--             if Map.null cs
--                 then return (req', cs)
--                 else return $ liftedNow >>= \t -> freshenCookies t req' True
--         return (req, cs')
--         (t', res) <- connWrap (reqTimeout req)
--                               (reqFailedConnection req)

connWrap :: Maybe Int -> e -> IO a -> IO (Maybe NominalDiffTime, Either e a)
connWrap Nothing _ f    = fmap (\x -> (Nothing, Right x)) f
connWrap (Just t) e f   = liftedNow >>= \t0 ->
    timeout t f >>= \res-> timeoutHandler (fromIntegral t) t0 res e

timeoutHandler :: MonadIO m
               => NominalDiffTime
               -> UTCTime
               -> Maybe a
               -> e
               -> m (Maybe NominalDiffTime, Either e a)
timeoutHandler _ _ Nothing e    = return (Nothing, Left e)
timeoutHandler t t0 (Just r) e  = liftedNow >>= \t1 ->
    let dt  = (diffUTCTime t1 t0)*1000*1000
        t'  = t - dt
    in  if t' <= 0
           then return (Nothing, Left e)
           else return (Just t', Right r)
