{-# LANGUAGE ScopedTypeVariables #-}

module ExceptionMiddleware
    ( handleException
    , handleExceptionPure
    , errorOnException
    , errorOnSomeException
    ) where

import Network.Wai ( responseLBS, Middleware, Request, Response )
import Control.Exception ( SomeException, catch, Exception )
import Data.Data ( Proxy(..) )
import Network.HTTP.Types.Status ( internalServerError500 )

import qualified Data.ByteString.Lazy.Char8 as BS

-- | General exception handler. Can respond to exception and request to perform IO actions and construct a response.
--
-- Exception types are controlled by a `Proxy` argument.
--
handleException :: Exception e => Proxy e -> (Request -> e -> IO Response) -> Middleware
handleException (_proxy :: Proxy e) f app req send =
  catch (app req send) $ \(e :: e) -> do
    res <- f req e
    send res

-- | General exception handler. Can respond to exception and request to construct a response.
--
-- Exception types are controlled by a `Proxy` argument.
--
handleExceptionPure :: Exception e => Proxy e -> (Request -> e -> Response) -> Middleware
handleExceptionPure p f = handleException p (\r e -> pure $ f r e)

-- | Exceptions are turned into error responses.
--
-- Exception types are controlled by a `Proxy` argument.
--
errorOnException :: Exception e => Proxy e -> Middleware
errorOnException p = handleExceptionPure p (\_r e -> responseLBS internalServerError500 [] (BS.pack (show e)))

-- | Exceptions are turned into error responses.
--
-- Catches all `SomeException` type exceptions.
--
errorOnSomeException :: Middleware
errorOnSomeException = errorOnException (Proxy :: Proxy SomeException)

