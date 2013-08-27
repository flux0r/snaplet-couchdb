{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.CouchDb.Http.Cookies where

import Data.List (sort)
import Data.Monoid(mappend, mempty)
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Types as H
import Control.Applicative (pure, (<*>))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Char8 as C
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Snap.Snaplet.CouchDb.Http.Types
import Snap.Snaplet.CouchDb.Utils
import Data.Time (UTCTime)
import Blaze.ByteString.Builder (Builder, toByteString, fromByteString)
import Blaze.ByteString.Builder.Char8 (fromChar)

domain, path :: Cookie -> ByteString
domain = maybeByteString id . cookieDomain
path = maybeByteString id . cookiePath

getCookieKey :: Cookie -> CookieKey
getCookieKey = pure (,,) <*> cookieName <*> domain <*> path

emptyCookies :: Cookies
emptyCookies = Map.empty

renderCookie :: (ByteString, ByteString) -> Builder
renderCookie (k, v) = fromByteString k `mappend` fromChar '='

renderCookies [] = mempty
renderCookies xs = foldr1 iter $ map renderCookie xs
  where
    iter x y = x `mappend` fromChar ';' `mappend` y

------------------------------------------------------------------------------
-- | Add cookies to a request by computing a cookie_string for the cookies
-- passed in and then updating the cookie header of the request. Return the
-- new request and the new cookie store, which has a new last-accessed-time
-- for each cookie.
addCookies :: Request p m a
           -> Cookies
           -> UTCTime                   -- ^ The time for last-accessed-time
           -> Bool                      -- ^ Is the request for a HTTP API?
           -> (Request p m a, Cookies)
addCookies req coks t httpApi = (if B.null cokStr then req else req', coks')
  where
    (cokStr, coks') = mkCookieString req coks t httpApi
    req' = req { reqHeaders = newHeaders }
    newHeaders = uncurry Map.insert (mkCookieHeader cokStr) $ reqHeaders req

mkCookieHeader :: a -> (H.HeaderName, a)
mkCookieHeader cokStr = (H.hCookie, cokStr)

matchingCookie :: Request p m a -> Bool -> Cookie -> Bool
matchingCookie req httpApi cok =
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
    validHttp = not (cookieHttpOnlyFlag cok) || httpApi

matchingCookies :: Request p m a
                -> Bool
                -> Cookies
                -> Cookies
matchingCookies = (Map.filter .) . matchingCookie

staleCookie :: UTCTime -> Cookie -> Bool
staleCookie t cok = maybe False (< t) (cookieExpires cok)

rmStaleCookies :: UTCTime -> Cookies -> Cookies
rmStaleCookies = Map.filter . staleCookie

freshenCookies :: UTCTime
               -> Request p m a
               -> Bool
               -> (Request p m a, Cookies)
freshenCookies now req http =
    addCookies req (rmStaleCookies now $ reqCookies req) now http

mkCookiePair :: Cookie -> (ByteString, ByteString)
mkCookiePair = pure (,) <*> cookieName <*> cookieValue

mkCookiePairs :: Cookies -> [(ByteString, ByteString)]
mkCookiePairs cs = map mkCookiePair $ sortedList cs
  where
    -- Data.Map sorts by keys, but RFC says to sort by cookie.
    sortedList = sort . Map.elems

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

insertCookie cok coks = Map.insert (getCookieKey cok) cok coks

exTest cok coks = return $ Map.insert (getCookieKey cok) cok coks
