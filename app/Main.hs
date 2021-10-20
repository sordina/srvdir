{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Maybe
import Options.Generic
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Application.Static
import System.FilePath
import ExceptionMiddleware
import Data.Default (def)
import System.IO.Unsafe (unsafePerformIO)

data Options = Options
             { port           :: Port
             , verbose        :: Bool
             , disableListing :: Bool
             , directory      :: Maybe FilePath
             } deriving (Show, Generic)

instance ParseRecord Options

mkLogger :: IO Middleware
mkLogger = mkRequestLogger def { outputFormat = DetailedWithSettings (def { mPrelogRequests = True }) }

serveDirectory :: Options -> FilePath -> Application
serveDirectory o = staticApp . settings . addTrailingPathSeparator
  where
  settings f = setting { ssListing = listing }
    where
    setting = defaultFileServerSettings f
    listing        | disableListing o = Nothing
                   | otherwise        = ssListing setting

runServer :: Options -> IO ()
runServer o = do
  logger <- mkLogger
  run p (s logger)
  where
  p = port o
  d = fromMaybe "." (directory o)
  s log | verbose o = log (errorOnSomeException (serveDirectory o d))
        | otherwise = serveDirectory o d

main :: IO ()
main = runServer =<< getRecord "srvdir - simple directory server!"
