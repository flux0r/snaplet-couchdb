{-# LANGUAGE RankNTypes #-}

module Snap.Snaplet.CouchDb.Http.Connection where

import Control.Category (Category)
import Data.ByteString (ByteString)
import qualified Data.Machine as M
import Data.Machine (MachineT, SourceT)
import qualified Data.ByteString as B
import Control.Monad.IO.Class (MonadIO, liftIO)

data Connection = Conn
    { connR :: IO ByteString
    , connW :: ByteString -> IO ()
    , connE :: IO ()
    }

connSrc :: MonadIO m => Connection -> SourceT m ByteString
connSrc c = M.repeatedly $ iter
  where
    r = connR c
    iter = liftIO r >>= \xs ->
        if B.null xs
            then M.stop
            else M.yield xs

connSnk :: (MonadIO m, Category k)
        => Connection
        -> MachineT m (k ByteString) o
connSnk c = M.repeatedly $ iter
  where
    w = connW c
    iter = M.await >>= liftIO . w
