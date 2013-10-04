{-# LANGUAGE RankNTypes #-}

module Snap.Snaplet.CouchDb.Http.LowLevel where

import Control.Concurrent (newMVar)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (liftM)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Foreign.C.Error (getErrno, Errno)
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt (..), CSize (..), CChar (..))
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Alloc (mallocBytes)
import Network.Socket (Family, SocketStatus (NotConnected), Socket (MkSocket),
                      SocketType, ProtocolNumber, fdSocket, packFamily,
                      packSocketType)
import qualified Network.Socket.ByteString as NS
import System.Posix.Types (Fd (Fd))
import System.Posix.Internals (setNonBlockingFD)

import qualified Data.Machine as M
import           Data.Machine (SourceT)

foreign import ccall unsafe "socket"
    c_socket :: CInt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "unistd.h read"
    c_read :: CInt -> Ptr CChar -> CSize -> IO CInt

foreign import ccall unsafe "recv"
    c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt

getSock :: Family
        -> SocketType
        -> ProtocolNumber
        -> IO (Either Errno Socket)
getSock fam typ pro =
    c_socket (packFamily fam) (packSocketType typ) pro >>= \fd ->
        if fd == -1
            then liftM Left getErrno
            else do
                setNonBlockingFD fd True
                sockStat <- newMVar NotConnected
                return . Right $ MkSocket fd fam typ pro sockStat

srcSocket :: MonadIO m
          => Socket
          -> Ptr CChar
          -> CSize
          -> M.SourceT m (Either Errno ByteString)
srcSocket s mem nbytes = M.repeatedly $ do
    x <- liftIO $ fdRecv fd mem nbytes
    case x of
        Right 0 -> M.stop
        Right n -> do
            str <- liftIO $ B.packCStringLen (mem, fromIntegral n)
            M.yield . Right $ str
        Left e  -> M.yield . Left $ e
  where
    fd = fdSocket s

data ConnResource = ConnResource
    { connMem       :: Ptr CChar
    , connSock      :: Socket
    , connBufSize   :: CSize
    }

checkForeign :: (Integral a, Num b) => a -> IO (Either Errno b)
checkForeign x =
    if x == -1
        then badVal
        else goodVal x

badVal :: IO (Either Errno a)
badVal = liftM Left getErrno

goodVal :: (Num b, Monad m, Integral a) => a -> m (Either a' b)
goodVal = return . Right . fromIntegral

fdRecv :: Num a => CInt -> Ptr CChar -> CSize -> IO (Either Errno a)
fdRecv fd ptr n = c_recv fd ptr n 0 >>= checkForeign

fdRead :: Num a => Fd -> Ptr CChar -> CSize -> IO (Either Errno a)
fdRead (Fd fd) ptr n = c_read fd ptr n >>= checkForeign
