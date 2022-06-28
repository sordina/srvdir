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
import MethodTranslator (allGet)
import Data.Default (def)

data Options = Options
             { port           :: Port
             , directory      :: Maybe FilePath
             , verbose        :: Bool
             , disableListing :: Bool
             , allMethods     :: Bool
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

boolId :: Bool -> (a -> a) -> (a -> a)
boolId b f
  | b         = f
  | otherwise = id

runServer :: Options -> IO ()
runServer o = do
  logger <- mkLogger
  let
    d = fromMaybe "." (directory o)
    logging = boolId (verbose    o) (logger . errorOnSomeException)
    getting = boolId (allMethods o) allGet
  run (port o) $ (getting . logging) (serveDirectory o d)

main :: IO ()
main = runServer =<< getRecord "srvdir - simple directory server!"
