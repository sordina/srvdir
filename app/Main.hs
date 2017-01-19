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


data Options = Options
             { port           :: Port
             , verbose        :: Bool
             , disableListing :: Bool
             , directory      :: Maybe FilePath
             } deriving (Show, Generic)

instance ParseRecord Options

serveDirectory :: Options -> FilePath -> Application
serveDirectory o = staticApp . settings . addTrailingPathSeparator
  where
  settings f = setting { ssListing = listing }
    where
    setting = defaultFileServerSettings f
    listing        | disableListing o = Nothing
                   | otherwise        = ssListing setting

runServer :: Options -> IO ()
runServer o = run p s
  where
  p = port o
  d = fromMaybe "." (directory o)
  s | verbose o = logStdoutDev (serveDirectory o d)
    | otherwise = serveDirectory o d

main :: IO ()
main = runServer =<< getRecord "srvdir - simple directory server!"
