module Snap.Snaplet.CouchDb.Utils (
    diffByteString,
    isIpAddress,
    ipv4Regex,
    maybeByteString
) where

import Control.Error
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import Text.Regex (Regex, mkRegex, matchRegex)

maybeByteString :: (a -> ByteString) -> Maybe a -> ByteString
maybeByteString = maybe B.empty

diffByteString :: ByteString -> ByteString -> ByteString
diffByteString x y = B.take (B.length x - B.length y) x

ipv4Regex :: Regex
ipv4Regex = mkRegex "^([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})$"

inIpRange :: Int -> Bool
inIpRange = \ x -> x >= 0 && x < 256

isIpAddress :: ByteString -> Bool
isIpAddress = maybe False verify . matchRegex ipv4Regex . U.toString
  where
    verify = \ xs -> length xs == 4 && all (inIpRange . read) xs
