{-
    hums - The Haskell UPnP Server
    Copyright (C) 2009,2012 Bardur Arantsson <bardur@scientician.net>

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

import           Control.Concurrent
import           Control.Monad.Error
import           Data.ConfigFile
import           Data.IORef
import           Data.UUID ()
import           Network.Utils
import           Network.Wai (Application, Request(..))
import           Network.Wai.Handler.Warp (runSettings,
                                           defaultSettings,
                                           settingsTimeout,
                                           settingsPort)
import           System.FilePath
import qualified System.UUID.V4 as U
import           Text.Printf

import Configuration
import Handlers
import Object
import Paths_hums
import Service
import SimpleServiceDiscoveryProtocol

defaultMediaServerConfiguration :: String -> MediaServerConfiguration
defaultMediaServerConfiguration uuid_ =
    MediaServerConfiguration { uuid = uuid_
                             , friendlyName = "hums"
                             , manufacturer = "Bardur Arantsson"
                             , manufacturerUrl = "http://www.scientician.net"
                             , modelDescription = Just "Haskell UPnP Media Server"
                             , modelName = "HUMS"
                             , modelNumber = "0"
                             , modelUrl = "http://www.scientician.net"
                             , serialNumber = Just "0123456789"
                             , upc = Nothing
                             }

periodicUpdate :: IORef a -> (IO a) -> IO ()
periodicUpdate a doUpdate =
  forever $ do
    threadDelay 120000000      -- Every 120 seconds should be more than enough.
    writeIORef a =<< doUpdate  -- Atomic replace

scanOnce :: String -> IO Objects
scanOnce directory = do
    putStrLn $ printf "Scanning directory '%s'" directory
    objects <- scanDirectory directory
    putStrLn $ printf "Scanning completed"
    return objects

application :: State -> String -> Application
application state dataDirectory request = do
  case pathInfo request of
    ["description.xml"] ->
      rootDescriptionHandler state
    ("static" : path) ->
      staticHandler request (dataDirectory </> "www") path
    ("dynamic" : "services" : "ContentDirectory" : "control" : _) ->
      serviceControlHandler state ContentDirectoryDevice request
    ("dynamic" : "services" : "ConnectionManager" : "control" : _) ->
      serviceControlHandler state ConnectionManagerDevice request
    ["content" , objectId ] ->
      contentHandler request state objectId
    _ ->
      fallbackHandler

main :: IO ()
main = niceSocketsDo $ do

  -- Get the data directory.
  dataDirectory <- getDataFileName "."

  -- Parse configuration.
  c <- parseConfiguration emptyCP $ dataDirectory </> "hums.cfg"
  print c

  -- Scan objects once at the start.
  let scanOnce' = scanOnce $ rootDirectory c
  defaultObjects_ <- scanOnce'
  defaultObjects <- newIORef defaultObjects_

  -- Get the application information.
  appInfo <- getApplicationInformation

  -- Build configurations, etc.
  u <- fmap show U.uuid
  putStrLn $ "My UUID is: " ++ u
  let mc = defaultMediaServerConfiguration u
  let services = [ ContentDirectoryDevice, ConnectionManagerDevice ]
  let st = (c,mc,appInfo,services, defaultObjects)

  -- Start serving.
  putStrLn "Establishing HTTP server..."
  _ <- forkIO $ do
         let settings = defaultSettings { settingsPort = (httpServerPort c)
                                        , settingsTimeout = 86400
                                        }
         runSettings settings (application st dataDirectory)
  _ <- putStrLn "Done."

  -- Start broadcasting alive messages.
  putStrLn "Establishing notification broadcaster..."
  _ <- forkIO $ sendNotifyForever appInfo c mc

  -- Start scanning files/directories in the background.
  putStrLn "Establishing background scanner..."
  _ <- forkIO $ periodicUpdate defaultObjects scanOnce'

  -- Wait for all threads to terminate.
  interact id

  return ()
