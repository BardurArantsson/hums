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

module HttpServer ( runHttpServer
                  ) where

import Network.HTTP.Base
import Network.HTTP.Stream
import Network.Socket
import Network.StreamSocket()
import Control.Monad
import Control.Concurrent
import Control.Exception
import Data.Word
import HttpMonad (HttpT, runHttp)
import Network.Socket.ByteString (sendAll)

-- Request handlers.
type RequestHandler =
    Socket -> Request String -> IO ()

runHttpServer' :: RequestHandler -> Word16 -> IO ()
runHttpServer' r p = do
  let p' = fromIntegral p :: PortNumber
  let sa = SockAddrInet p' iNADDR_ANY
  s <- socket AF_INET Stream 0
  setSocketOption s ReuseAddr 1
  bindSocket s sa
  listen s 5
  forever $ acceptConnection s r

acceptConnection :: Socket -> RequestHandler -> IO ()
acceptConnection listenSocket r = do
  (s,_) <- accept listenSocket
  _ <- forkIO $ bracket
             (return s)
             sClose
             (handleHttpConnection r)
  return ()  -- Don't care about the thread ID.

handleHttpConnection :: RequestHandler -> Socket -> IO ()
handleHttpConnection r c = do
    result <- receiveHTTP c
    case result of
      Left _ -> error "Error reading request"
      Right req -> r c req

runHttpServer :: HttpT IO () -> Word16 -> IO ()
runHttpServer handler = runHttpServer' myRequestHandler
  where
    myRequestHandler :: Socket -> Request String -> IO ()
    myRequestHandler s r = runHttp (sendAll s) r $ handler
