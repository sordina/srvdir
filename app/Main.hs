{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Maybe
import Options.Generic
import Servant.Utils.StaticFiles
import Servant.API.Raw
import Servant.Server
import Servant
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger

data Options = Options
             { port      :: Port
             , verbose   :: Bool
             , directory :: Maybe FilePath
             } deriving (Show, Generic)

instance ParseRecord Options

server :: FilePath -> Application
server f = serve (Proxy :: Proxy Raw) (serveDirectory f)

runServer :: Options -> IO ()
runServer o = run p s
  where
  p = port o
  d = fromMaybe "." (directory o)
  s | verbose o = logStdoutDev (server d)
    | otherwise = server d

main :: IO ()
main = runServer =<< getRecord "srvdir - simple servant directory server!"
