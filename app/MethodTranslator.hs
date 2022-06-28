{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MethodTranslator
    ( translateMethods
    , allGet
    ) where

import Network.Wai ( responseLBS, Middleware, Request, Response, requestMethod )
import Control.Exception ( SomeException, catch, Exception )
import Data.Data ( Proxy(..) )
import Network.HTTP.Types as T

import qualified Data.ByteString.Lazy.Char8 as BS

-- | Translate HTTP methods before execution.
--
translateMethods ::  (T.Method -> T.Method) -> Middleware
translateMethods f app req send = app req { requestMethod = f ( requestMethod req ) } send

allGet :: Middleware
allGet = translateMethods (const "GET")

