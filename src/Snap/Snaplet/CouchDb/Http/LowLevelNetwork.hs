{-# LANGUAGE RankNTypes #-}

import Control.Concurrent (newMVar)
import Control.Monad.IO.Class (liftIO, MonadIO)
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

main = undefined

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
            then getErrno >>= return . Left
            else do
                setNonBlockingFD fd True
                sockStat <- newMVar NotConnected
                return . Right $ MkSocket fd fam typ pro sockStat

source sock = M.repeatedly $ do
    bs <- liftIO $ NS.recv sock 4096
    if B.null bs
        then M.stop
        else M.yield bs

srcSock :: MonadIO m
        => Socket
        -> Ptr CChar
        -> CSize
        -> M.SourceT m (Either Errno ByteString)
srcSock s mem nbytes = M.repeatedly $ do
    x <- liftIO $ fdRecv fd mem nbytes
    case x of
        Right 0 -> M.stop
        Right n -> do
            str <- liftIO $ B.packCStringLen (mem, fromIntegral n)
            M.yield . Right $ str
        Left e  -> M.yield . Left $ e
  where
    fd = fdSocket s

checkForeign :: (Integral a) => a -> IO (Either Errno Int)
checkForeign x =
    if x == -1
        then getErrno >>= return . Left
        else return . Right . fromIntegral $ x

fdRecv :: CInt -> Ptr CChar -> CSize -> IO (Either Errno Int)
fdRecv fd ptr n = c_recv fd ptr n 0 >>= checkForeign

fdRead :: Fd -> Ptr CChar -> CSize -> IO (Either Errno Int)
fdRead (Fd fd) ptr n = c_read fd ptr n >>= checkForeign
