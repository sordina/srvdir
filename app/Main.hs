{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Data.Maybe
import Options.Generic
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Application.Static
import Network.Wai.Middleware.Cors
import System.FilePath
import ExceptionMiddleware
import MethodTranslator (allGet)
import Data.Default (def)
import Control.Monad (when)

data Options = Options
             { port           :: Port
             , directory      :: Maybe FilePath
             , verbose        :: Bool
             , disableListing :: Bool
             , allMethods     :: Bool
             , cors           :: Bool
             } deriving (Show, Generic)

instance ParseRecord Options

mkLogger :: IO Middleware
mkLogger = mkRequestLogger def { outputFormat = DetailedWithSettings (def { mPrelogRequests = True }) }

serveDirectory :: Options -> FilePath -> Application
serveDirectory Options { disableListing, cors } = app . settings . addTrailingPathSeparator
  where
  app s | cors = simpleCors (staticApp s)
        | otherwise = staticApp s
  settings f = setting { ssListing = listing }
    where
    setting = defaultFileServerSettings f
    listing        | disableListing = Nothing
                   | otherwise      = ssListing setting

boolId :: Bool -> (a -> a) -> (a -> a)
boolId b f
  | b         = f
  | otherwise = id

runServer :: Options -> IO ()
runServer o@Options {..} = do
  logger <- mkLogger
  let
    d       = fromMaybe "." directory
    logging = boolId verbose    (logger . errorOnSomeException)
    getting = boolId allMethods allGet
  when verbose do
    putStrLn "Running srvdir with options:"
    print o
  run port $ (logging . getting) (serveDirectory o d)

main :: IO ()
main = runServer =<< getRecord "srvdir - simple directory server!"
