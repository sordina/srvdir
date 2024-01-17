{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module ReflectContentTypeMiddleware
    ( reflectContentTypeMiddleware
    ) where

import Network.Wai ( responseLBS, Middleware, Request, Response, mapResponseHeaders, modifyResponse, requestHeaders )
import Control.Exception ( SomeException, catch, Exception )
import Data.Data ( Proxy(..) )
import Network.HTTP.Types.Status ( internalServerError500 )
import Network.HTTP.Types.Header ( HeaderName )
import Data.List (find)

import qualified Data.ByteString as BS

addHeader :: HeaderName -> BS.ByteString -> Response -> Response
addHeader n h = mapResponseHeaders (((n,h) : ) . filter ((/= "content-type") . fst))

accept :: Request -> Maybe BS.ByteString
accept r = snd <$> find ( (== "accept") . fst ) (requestHeaders r)

-- | Picks the first accept content type and sets it as the response content type
--
reflectContentTypeMiddleware :: Middleware
reflectContentTypeMiddleware app req send =
  case accept req of
    Nothing -> app req send
    Just a -> modifyResponse (addHeader "content-type" a) app req send

