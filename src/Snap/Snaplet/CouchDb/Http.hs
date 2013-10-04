module Snap.Snaplet.CouchDb.Http where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Pool (takeResource)
import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)
import Control.Exception (throwIO)
import Snap.Snaplet.CouchDb.Http.Cookies (freshenCookies)
import Snap.Snaplet.CouchDb.Http.Types (RequestBody (RequestBodyProducer),
                                        Request (..), HttpError)
import Snap.Snaplet.CouchDb.Http.Request (reqFailedConnection)
import Snap.Snaplet.CouchDb.Utils (liftedNow)
import Snap.Snaplet.CouchDb.Http.LowLevel (ConnResource(..), srcSocket)
import qualified Data.Map as Map
import System.Timeout (timeout)

httpNoRedirects req' pool = let cs = reqCookies req' in
    do
        (req, cs') <- return $
            if Map.null cs
                then (req', cs)
                else liftedNow >>= \t -> freshenCookies t req' True
        return (req, cs')
        (t', res) <- connWrap (reqTimeout req)
                              (reqFailedConnection req)
                              (takeResource pool)
        case res of
            Right (conn, lclPool) ->
                let src = srcSocket (connSock conn)
                                    (connMem conn)
                                    (connBufSize conn)
                in  return (t', src)

connWrap :: MonadIO m
         => Maybe Int
         -> e
         -> IO a
         -> m (Maybe NominalDiffTime, Either e a)
connWrap Nothing _ f    = liftIO $ fmap (\x -> (Nothing, Right x)) f
connWrap (Just t) e f   = liftIO $ liftedNow >>= \t0 ->
    timeout t f >>= \res-> timeoutHandler (fromIntegral t) t0 res e

timeoutHandler :: MonadIO m
               => NominalDiffTime
               -> UTCTime
               -> Maybe a
               -> e
               -> m (Maybe NominalDiffTime, Either e a)
timeoutHandler _ _ Nothing e    = return (Nothing, Left e)
timeoutHandler t t0 (Just r) e  = liftedNow >>= \t1 ->
    let dt  = diffUTCTime t1 t0 * 1000000
        t'  = t - dt
    in  return $
            if t' <= 0
               then (Nothing, Left e)
               else (Just t', Right r)
