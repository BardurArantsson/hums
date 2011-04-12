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

import Network.Utils
import SimpleServiceDiscoveryProtocol
import Configuration
import Control.Concurrent
import Service
import Text.Printf
import qualified Data.UUID as U
import qualified Data.UUID.V1 as U1
import Object
import Control.Monad.Error
import System.FilePath
import Data.Maybe (fromJust)
import Data.ConfigFile
import Paths_hums
import Data.IORef
import Handlers
import Network.Wai (Application, Response, Request(..))
import Network.Wai.Handler.Warp (run)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Enumerator (Iteratee)

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

handleIf :: (Maybe a) -> (a -> Iteratee ByteString IO Response) -> Iteratee ByteString IO Response -> Iteratee ByteString IO Response
handleIf Nothing  _     noMatch = noMatch
handleIf (Just x) match _       = match x

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
  u <- fmap (U.toString . fromJust) U1.nextUUID
  putStrLn $ "My UUID is: " ++ u
  let mc = defaultMediaServerConfiguration u
  let services = [ ContentDirectoryDevice, ConnectionManagerDevice ]
  let st = (c,mc,appInfo,services, defaultObjects)

  let myApplication :: Application
      myApplication r =
        ifPath "/description.xml" (\_ -> rootDescriptionHandler st) $
        ifPrefix "/static/" (staticHandler r $ dataDirectory </> "www") $
        ifPrefix "/dynamic/services/ContentDirectory/control" (serviceControlHandler st ContentDirectoryDevice) $
        ifPrefix "/dynamic/services/ConnectionManager/control" (serviceControlHandler st ConnectionManagerDevice) $
        ifPrefix "/content/" (contentHandler r st) $
        fallbackHandler
          where
            ifPath p t f = handleIf (isPath p) t f
            ifPrefix p t f = handleIf (isPrefix p) t f
            isPath p = if rawPathInfo r == p then Just () else Nothing
            isPrefix p | B.isPrefixOf p $ rawPathInfo r = Just $ B.drop (B.length p) $ rawPathInfo r
            isPrefix _ = Nothing

  -- Start serving.
  putStrLn "Establishing HTTP server..."
  _ <- forkIO $ run (httpServerPort c) myApplication
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
