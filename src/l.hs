{-# LANGUAGE RankNTypes #-}

import Control.Monad (liftM, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.ByteString.Internal (createAndTrim)
import Data.Word (Word8)
import Foreign.C.Error (Errno, getErrno, errnoToIOError)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.C.Types (CInt (CInt), CSize (CSize), CChar (CChar))
import Network.Socket (Socket, fdSocket)
import qualified Pipes as P
import Pipes (Producer')

foreign import ccall unsafe "recv"
    c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt

socketProducer :: MonadIO m => Socket -> Int -> Producer' ByteString m ()
socketProducer s nbytes = unless (nbytes <= 0) iter
  where
    iter        = liftIO (createAndTrim nbytes $
                         recvHelper fd c_nbytes) >>= \bs -> 
                         unless (B.null bs) (P.yield bs >> iter)
    fd          = fdSocket s
    c_nbytes    = fromIntegral nbytes

recvHelper :: CInt -> CSize -> Ptr Word8 -> IO Int
recvHelper fd nbytes mem = c_recv fd (castPtr mem) nbytes 0 >>= \bs ->
        case fromIntegral bs of
            -1  -> getErrno >>= errHelper
            x   -> return x
  where
    errHelper e = ioError $ errnoToIOError "Low level" e Nothing Nothing
