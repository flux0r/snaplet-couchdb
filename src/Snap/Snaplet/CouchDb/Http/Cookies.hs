{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.CouchDb.Http.Cookies (
    addCookies,
    emptyCookies,
    getCookieKey,
    rmExpiredCookies
) where

import Control.Applicative (pure, (<*>))
import qualified Data.Map as Map
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Char8 as C
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Snap.Snaplet.CouchDb.Http.Types
import Snap.Snaplet.CouchDb.Utils

domain, path :: Cookie -> ByteString
domain = maybeByteString id . cookieDomain
path = maybeByteString id . cookiePath

getCookieKey :: Cookie -> CookieKey
getCookieKey = pure (,,) <*> cookieName <*> domain <*> path

emptyCookies :: Cookies
emptyCookies = Map.empty

addCookies = undefined

rmExpiredCookies = undefined

instance Eq Cookie where
    x == y = getCookieKey x == getCookieKey y

instance Ord Cookie where
    compare x y
      | B.length (path x) > B.length (path y)   = LT
      | B.length (path x) < B.length (path y)   = GT
      | otherwise                               = timeComparison
      where
        timeComparison = case (cookieCreated x, cookieCreated y) of
            (Just tx, Just ty)      -> compare tx ty
            otherwise               -> LT

validCookieDomain :: Cookie -> ByteString -> Bool
validCookieDomain cok host =
    cookieDomainHostEq cok host || cookieDomainHostMatch cok host

cookieDomainHostEq :: Cookie -> ByteString -> Bool
cookieDomainHostEq = flip (maybe False . (==)) . cookieDomain

cookieDomainHostMatch :: Cookie -> ByteString -> Bool
cookieDomainHostMatch = domainMatches . domain

domainMatches :: ByteString -> ByteString -> Bool
domainMatches x y
  | x == y                              = True
  | B.length x < B.length y + 1         = False
  | y `B.isSuffixOf` x &&
    lastChar == U.fromString "." &&
    not (isIpAddress x)                 = True
  | otherwise                           = False
  where
    lastChar = B.singleton (B.last $ diffByteString x y)

mkUriPath :: Request p m a -> ByteString
mkUriPath req =
    if invalid uriPath
        then slash
        else B.reverse $ B.tail $ C.dropWhile (/= '/') $ B.reverse uriPath
  where
    uriPath     = reqPath req
    slash       = U.fromString "/"
    invalid x   = B.null x
                    || B.singleton (B.head x) /= slash
                    || C.count '/' x <= 1
