{-
    hums - The Haskell UPnP Server
    Copyright (C) 2009, 2012 Bardur Arantsson <bardur@scientician.net>

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

import           Control.Monad.Error
import           Data.ConfigFile
import           Data.Either.Utils
import           Data.Maybe
import           Filesystem.Path (FilePath)
import           Filesystem.Path.CurrentOS (decodeString, encodeString)
import           Network.URI (URI, parseURI)
import           Prelude hiding (FilePath)
import           System.Posix.Unistd (SystemID(..), getSystemID)
import           Text.Printf

{-

   Configuration

-}

data Configuration =
    Configuration { localNetIp :: String
                  , httpServerBase :: URI
                  , httpServerPort :: Int
                  , enableDeviceIcon :: Bool
                  , useDlna :: Bool
                  , dlnaProfileName :: Maybe String -- Only used when useDlna is available.
                  , rootDirectory :: FilePath
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
  return ApplicationInformation
             { operatingSystemName = systemName systemId
             , operatingSystemVersion = release systemId
             , applicationName = "hums"
             , applicationVersion = "0"
             }

getServerHeaderValue :: ApplicationInformation -> String
getServerHeaderValue (ApplicationInformation on ov an av) =
    printf "%s/%s, UPnP/1.0, %s/%s" on ov an av

parseConfiguration :: ConfigParser -> FilePath -> IO Configuration
parseConfiguration cp0 cfname = do
  cfg <- runErrorT $ do
           cf <- join $ liftIO $ readfile cp0 (encodeString cfname)
           ip <- get cf networkSection "listen_ip"
           port <- get cf networkSection "listen_port"
           rootDirectory_ <- get cf defaultSection "root_directory"
           return Configuration
                      { localNetIp = ip
                      , httpServerPort = port
                      , httpServerBase = fromJust $ parseURI $ printf "http://%s:%d" ip port
                      , enableDeviceIcon = True
                      , useDlna = True
                      , dlnaProfileName = Nothing
                      , rootDirectory = decodeString rootDirectory_ }
  return $ forceEither cfg
  where
    defaultSection = "DEFAULT"
    networkSection = "network"
