import Blaze.ByteString.Builder (Builder)
import Control.Applicative ((*>), (<*), (<$>), (<*>))
import Data.Attoparsec.ByteString.Char8 (Parser, satisfy, string, char,
                                         decimal, takeTill)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI, mk, original)
import Data.Foldable (foldMap)
import Data.Int (Int64)
import Data.Traversable (Traversable)
import Data.HashMap.Strict (HashMap, foldrWithKey)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid (mempty, mconcat))
import Pipes (Consumer, Consumer', Pipe, Producer, (>->), runEffect, await, yield)
import Pipes.ByteString (fromLazy)

import qualified Blaze.ByteString.Builder as BU
import qualified Blaze.ByteString.Builder.Char8 as BC8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8

data Method = Get
            | Head
            | Post
            | Put
            | Delete
            | Trace
            | Options
            | Connect
            | Patch
            | Extension ByteString

instance Show Method where
    show Get            = "GET"
    show Head           = "HEAD"
    show Post           = "POST"
    show Put            = "PUT"
    show Delete         = "DELETE"
    show Trace          = "TRACE"
    show Options        = "OPTIONS"
    show Connect        = "CONNECT"
    show Patch          = "PATCH"
    show (Extension x)  = show x

newtype Headers = Hdrs { unHdrs :: HashMap (CI ByteString) ByteString }

data Connection = Conn
    { connHost      :: ByteString
    , connClose     :: IO ()
    , connR         :: Producer Builder IO ()
    , connW         :: Consumer ByteString IO ()
    }

instance Show Connection where
    show c = mconcat ["Host: ", C8.unpack . connHost $ c,  "\n"]

data Request = Req
    { reqMethod     :: Method
    , reqHost       :: Maybe ByteString
    , reqPath       :: ByteString
    , reqBody       :: EntityBody
    , reqContinue   :: ContinueStatus
    , reqHeaders    :: Headers
    }

data EntityBody = Empty
                | Chunked
                | Static Int64

data ContinueStatus = Normal
                    | Continue

sendRequest c req = let h = connHost c in
    let consumer    = connW c
        producer    = yield $ reqBytes req h
    in (producer, consumer)

readResponseHeader = undefined

readHeaderFields :: Consumer' ByteString IO [ByteString]
readHeaderFields = iter id >>= \f -> return $ f []
  where
    iter xs = await >>= \l ->
        if C8.null l
            then return xs
            else iter (xs . (l:))

parseStatusLine :: Parser (Int, ByteString)
parseStatusLine = (,) <$> statusCode <*> reasonPhrase
  where
    statusCode      =    (string . C8.pack $ "HTTP/1.")
                      *> satisfy httpVersion
                      *> char ' '
                      *> decimal <* char ' '
    reasonPhrase    = takeTill (== '\r') <* crlfP
    httpVersion c   = c == '1' || c == '0'

crlfP :: Parser ByteString
crlfP = string . C8.pack $ "\r\n"

reqBytes :: Request -> ByteString -> Builder
reqBytes req h = mconcat [reqLine, hLine, headersLine, crlfB]
  where
    method      = methodB . reqMethod $ req
    uri         = BU.copyByteString . reqPath $ req
    reqLine     = mconcat [method, spaceB, uri, spaceB,
                           httpVersionB, crlfB]
    hLine       = hostLineB h req
    headersLine = headersB . unHdrs . reqHeaders $ req

headersB :: HashMap (CI ByteString) ByteString -> Builder
headersB = foldrWithKey iter mempty
  where
    iter k v r = mconcat
        [ r
        , BU.copyByteString . original $ k
        , BC8.fromString ": "
        , BU.fromByteString v
        , crlfB
        ]


methodB :: Method -> Builder
methodB = BC8.fromString . show

httpVersionB :: Builder
httpVersionB = BC8.fromString "HTTP/1.1"

hostNameB :: ByteString -> Request -> Builder
hostNameB h = BU.copyByteString . fromMaybe h . reqHost

crlfB :: Builder
crlfB = BC8.fromString "\r\n"

spaceB :: Builder
spaceB = BC8.fromString " "

hostLineB :: ByteString -> Request -> Builder
hostLineB h req = mconcat [BC8.fromString "Host: ", hostNameB h req, crlfB]
