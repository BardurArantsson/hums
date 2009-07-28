{-
    hums - The Haskell UPnP Server
    Copyright (C) 2009 Bardur Arantsson <bardur@scientician.net>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Configuration ( Configuration(..)
                     , MediaServerConfiguration(..)
                     , ApplicationInformation
                     , getServerHeaderValue
                     , getApplicationInformation
                     , parseConfiguration
                     ) where

import Data.Word
import System.Posix.Unistd (SystemID(..), getSystemID)
import Text.Printf
import Network.URI (URI, parseURI)
import Data.ConfigFile
import Control.Monad.Error
import Data.Either.Utils
import Data.Maybe

{-

   Configuration

-}

data Configuration = 
    Configuration { local_net_ip :: String
                  , httpServerBase :: URI
                  , http_server_port :: Word16
                  , enableDeviceIcon :: Bool
                  , useDlna :: Bool
                  , dlnaProfileName :: Maybe String -- Only used when useDlna is available.
                  , rootDirectory :: String 
                  }
    deriving (Show)

{-

   Media Server Configuration 

-}

data MediaServerConfiguration = MediaServerConfiguration 
    { uuid :: String
    , friendlyName :: String
    , manufacturer :: String
    , manufacturerUrl :: String
    , modelDescription :: Maybe String
    , modelName :: String
    , modelNumber :: String
    , modelUrl :: String
    , serialNumber :: Maybe String
    , upc :: Maybe String
    }

{-

   Application and operating system information.

-}

data ApplicationInformation = ApplicationInformation 
    { operatingSystemName :: String
    , operatingSystemVersion :: String
    , applicationName :: String
    , applicationVersion :: String 
    }


getApplicationInformation :: IO ApplicationInformation 
getApplicationInformation = do
  systemId <- getSystemID
  return $ ApplicationInformation 
             { operatingSystemName = systemName systemId 
             , operatingSystemVersion = release systemId
             , applicationName = "hums"
             , applicationVersion = "0"
             }

getServerHeaderValue :: ApplicationInformation -> String
getServerHeaderValue (ApplicationInformation on ov an av) = 
    printf "%s/%s, UPnP/1.0, %s/%s" on ov an av

parseConfiguration :: ConfigParser -> String -> IO Configuration
parseConfiguration cp0 cfname = do
  cfg <- runErrorT $ do
           cf <- join $ liftIO $ readfile cp0 cfname
           ip <- get cf networkSection "listen_ip"
           port <- get cf networkSection "listen_port"
           rootDirectory_ <- get cf defaultSection "root_directory"
           return $ Configuration 
                  { local_net_ip = ip
                  , http_server_port = port
                  , httpServerBase = fromJust $ parseURI $ "http://" ++ ip ++ ":" ++ (show port)
                  , enableDeviceIcon = True
                  , useDlna = True
                  , dlnaProfileName = Nothing
                  , rootDirectory = rootDirectory_ }
  return $ forceEither cfg
  where
    defaultSection = "DEFAULT"
    networkSection = "network"