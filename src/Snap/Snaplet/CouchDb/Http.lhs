module Snap.Snaplet.CouchDb.Http where

import Data.Time.Clock (diffUTCTime)
import Control.Exception.Lifted (throwIO)
import Snap.Snaplet.CouchDb.Http.Cookies (freshenCookies)
import Snap.Snaplet.CouchDb.Http.Types (RequestBody (RequestBodyProducer),
                                        Request (..))
import Snap.Snaplet.CouchDb.Utils (liftedNow)
import qualified Data.Map as Map
import System.Timeout.Lifted (timeout)

httpNoRedirects req' resource = let cs = reqCookies req' in
    do
        (req, cs') <-
            if Map.null cs
                then return (req', cs)
                else return $ liftedNow >>= \t -> freshenCookies t req' True
        return (req, cs')

-- connWrap Nothing _ f = fmap (\x -> (Nothing, Right x)) f
-- connWrap (Just timeout') e f = liftedNow >>= \t0 ->
--     timeout timeout' f >>= \res->
--     timeoutHandler t0 res e

timeoutHandler _ _ Nothing e      = (Nothing, Left e)
timeoutHandler t t0 (Just r) e    = liftedNow >>= \t1 ->
    let timeSpent = (diffUTCTime t1 t0)*1000*1000
        timeLeft  = round $ fromIntegral t - timeSpent
    in  if timeLeft <= 0
            then (Nothing, Left e)
            else (Just timeLeft, Right r)
