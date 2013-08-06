{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.CouchDb.Http.Cookies (
    addCookies,
    emptyCookies,
    getCookieKey,
    rmStaleCookies
) where

import Control.Applicative (pure, (<*>))
import qualified Data.Map as Map
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Char8 as C
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Snap.Snaplet.CouchDb.Http.Types
import Snap.Snaplet.CouchDb.Utils
import Data.Time (UTCTime)
import Web.Cookie (renderCookies)
import Blaze.ByteString.Builder (toByteString)

domain, path :: Cookie -> ByteString
domain = maybeByteString id . cookieDomain
path = maybeByteString id . cookiePath

getCookieKey :: Cookie -> CookieKey
getCookieKey = pure (,,) <*> cookieName <*> domain <*> path

emptyCookies :: Cookies
emptyCookies = Map.empty

addCookies = undefined

matchingCookie :: Request p m a -> Bool -> Cookie -> Bool
matchingCookie req httpReq cok =
    validHost && validPath && validSecure && validHttp
  where
    dom = cookieDomain cok
    host = reqHost req
    path = reqPath req
    validHost = if cookieHostOnlyFlag cok
                    then maybe False (== host) dom
                    else maybe False (domainMatches host) dom
    validPath = maybe False (pathMatches host) dom
    validSecure = not (cookieSecureOnlyFlag cok) || reqSecure req
    validHttp = not (cookieHttpOnlyFlag cok) || httpReq

matchingCookies :: Request p m a -> Bool -> Cookies -> Cookies
matchingCookies = (Map.filter .) . matchingCookie

staleCookie :: UTCTime -> Cookie -> Bool
staleCookie t cok = maybe False (< t) (cookieExpires cok)

rmStaleCookies :: UTCTime -> Cookies -> Cookies
rmStaleCookies = Map.filter . staleCookie

mkCookiePair :: Cookie -> (ByteString, ByteString)
mkCookiePair = pure (,) <*> cookieName <*> cookieValue

mkCookiePairs :: Cookies -> [(ByteString, ByteString)]
mkCookiePairs cs = Map.foldr ((:) . mkCookiePair) [] cs

updateAccessTime :: UTCTime -> Cookie -> Cookie
updateAccessTime t cok = cok { cookieLastAccessed = Just t }

updateAccessTimes :: UTCTime -> Cookies -> Cookies
updateAccessTimes = Map.map . updateAccessTime

mkCookieString :: Request p m a
               -> Cookies
               -> UTCTime
               -> Bool
               -> (ByteString, Cookies)
mkCookieString req coks t httpReq =
    (cookieString matches, updateAccessTimes t matches)
  where
    cookieString = toByteString . renderCookies . mkCookiePairs
    matches = matchingCookies req httpReq coks

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

slash :: ByteString
slash = U.fromString "/"

validCookieDomain :: Cookie -> ByteString -> Bool
validCookieDomain cok host =
    cookieDomainHostEq cok host || cookieDomainHostMatch cok host

cookieDomainHostEq :: Cookie -> ByteString -> Bool
cookieDomainHostEq = flip (maybe False . (==)) . cookieDomain

cookieDomainHostMatch :: Cookie -> ByteString -> Bool
cookieDomainHostMatch = domainMatches . domain

domainMatches :: ByteString -> ByteString -> Bool
domainMatches host dom
  | host == dom                         = True
  | B.length host < B.length dom + 1    = False
  | dom `B.isSuffixOf` host &&
    lastChar == U.fromString "." &&
    not (isIpAddress host)              = True
  | otherwise                           = False
  where
    lastChar = B.singleton (B.last $ diffByteString host dom)

mkUriPath :: ByteString -> ByteString
mkUriPath uriPath =
    if invalid uriPath
        then slash
        else B.reverse $ B.tail $ C.dropWhile (/= '/') $ B.reverse uriPath
  where
    invalid x   = B.null x
                    || B.singleton (B.head x) /= slash
                    || C.count '/' x <= 1

pathMatches :: ByteString -> ByteString -> Bool
pathMatches urip cokp
  | cokp == urip = True
  | cokp `B.isPrefixOf` urip
        && (B.singleton (B.last cokp) == slash
                || B.singleton (B.head remainder) == slash) = True
  | otherwise = False
  where
    remainder = B.drop (B.length cokp) urip
