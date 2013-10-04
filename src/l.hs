{-# LANGUAGE RankNTypes #-}

import Control.Concurrent (newMVar)
import Control.Monad (liftM, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.ByteString.Internal (createAndTrim)
import qualified Data.Pool as Poo
import Data.Pool (Pool)
import Data.Time.Clock (NominalDiffTime)
import Data.Word (Word8)
import Foreign.C.Error (Errno, getErrno, errnoToIOError)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.C.Types (CInt (CInt), CSize (CSize), CChar (CChar))
import qualified Network.Socket as NS
import Network.Socket (Family, ProtocolNumber, Socket, SocketType)
import qualified Pipes as P
import Pipes (Producer')
import System.Posix.Internals (setNonBlockingFD)

socketProducer :: MonadIO m => Socket -> Int -> Producer' ByteString m ()
socketProducer s nbytes = unless (nbytes <= 0) iter
  where
    iter        = liftIO (createAndTrim nbytes $
                         recvHelper fd c_nbytes) >>= \bs -> 
                         unless (B.null bs) (P.yield bs >> iter)
    fd          = NS.fdSocket s
    c_nbytes    = fromIntegral nbytes

recvHelper :: CInt -> CSize -> Ptr Word8 -> IO Int
recvHelper fd nbytes mem = c_recv fd (castPtr mem) nbytes 0 >>= \bs ->
        case fromIntegral bs of
            -1  -> getErrno >>= errHelper "Low level: recv"
            x   -> return x

foreign import ccall unsafe "recv"
    c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt

errHelper msg e = ioError $ errnoToIOError msg e Nothing Nothing

getSockRaw :: Family -> SocketType -> CInt -> IO Socket
getSockRaw fam typ pro =
    c_socket (NS.packFamily fam) (NS.packSocketType typ) pro >>= \fd ->
        if fd == -1
            then getErrno >>= errHelper "Low level: socket"
            else setNonBlockingFD fd True >>
                 newMVar NS.NotConnected >>= \socketStatus ->
                 return $ NS.MkSocket fd fam typ pro socketStatus

foreign import ccall unsafe "socket"
    c_socket :: CInt -> CInt -> CInt -> IO CInt

getSock :: IO Socket
getSock = getSockRaw NS.AF_INET NS.Stream NS.defaultProtocol

socketPoolRaw :: Int -> NominalDiffTime -> Int -> IO (Pool Socket)
socketPoolRaw = Poo.createPool getSock NS.sClose

socketPool :: IO (Pool Socket)
socketPool = socketPoolRaw 1 10 20
