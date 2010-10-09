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

module SimpleServiceDiscoveryProtocol ( sendNotifyForever
                                      , MessageType(..)
                                      )
    where

import Configuration
import Network.Socket
import Text.Printf
import URIExtra
import Control.Concurrent
import Control.Monad

-- Notification message types.
data MessageType = UpnpServiceNotification
                 | RootDeviceNotification
                 | ConnectionManagerNotification
                 | ContentDirectoryNotification
                 | MediaServerNotification

generateNotifyAlive :: ApplicationInformation -> Configuration -> MediaServerConfiguration -> MessageType -> String
generateNotifyAlive ai c msc messageType =
    concat [ printf "NOTIFY * HTTP/1.1\r\n"
           , printf "HOST: 239.255.255.250:1900\r\n"
           , printf "CACHE-CONTROL: max-age=180\r\n"
           , printf "LOCATION: %s\r\n" base_url
           , printf "NT: %s\r\n" $ messageTypeAsString messageType
           , printf "NTS: ssdp:alive\r\n"
           , printf "SERVER: %s\r\n" (getServerHeaderValue ai)
           , printf "USN: uuid:%s%s\r\n" (uuid msc) messageTypeSuffix
           , printf "\r\n" ]
    where
      base_url = show $ mkURI ["description.xml"] $ httpServerBase c
      messageTypeAsString UpnpServiceNotification = printf "uuid:%s" $ uuid msc
      messageTypeAsString RootDeviceNotification = "upnp:rootdevice"
      messageTypeAsString ConnectionManagerNotification = "urn:schemas-upnp-org:service:ConnectionManager:1"
      messageTypeAsString ContentDirectoryNotification = "urn:schemas-upnp-org:service:ContentDirectory:1"
      messageTypeAsString MediaServerNotification = "urn:schemas-upnp-org:device:MediaServer:1"
      messageTypeSuffix = case messageType of
                            UpnpServiceNotification -> ""
                            _ -> printf "::%s" $ messageTypeAsString messageType

sendRawMessage :: Configuration -> String -> IO ()
sendRawMessage c m = do
  -- Addresses
  sa <- inet_addr $ localNetIp c
  da <- inet_addr "239.255.255.250"
  -- Open socket and send
  sock <- socket AF_INET Datagram 0
  bindSocket sock $ SockAddrInet aNY_PORT sa  -- Use local interface
  _ <- sendTo sock m (SockAddrInet 1900 da)        -- Ignore return value
  sClose sock
  return ()

sendNotifyAlive :: ApplicationInformation -> Configuration -> MediaServerConfiguration -> MessageType -> IO ()
sendNotifyAlive ai c msc =
    sendRawMessage c . generateNotifyAlive ai c msc

sendNotifyAliveAll :: ApplicationInformation -> Configuration -> MediaServerConfiguration -> IO ()
sendNotifyAliveAll ai c msc = do
    sendIt UpnpServiceNotification
    sendIt RootDeviceNotification
    sendIt MediaServerNotification
    sendIt ContentDirectoryNotification
    sendIt ConnectionManagerNotification
    where
      sendIt mt = do
        sendNotifyAlive ai c msc mt
        threadDelay 100000

sendNotifyForever :: ApplicationInformation -> Configuration -> MediaServerConfiguration -> IO ()
sendNotifyForever ai c msc =
  forever $ do
    putStrLn "Broadcasting alive notifications..."
    sendNotifyAliveAll ai c msc
    threadDelay 30000000           -- Every 30 seconds should be more than enough.
