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
                  , UrlHandler
                  , sendHeaders
                  , sendErrorResponse
                  , sendOkHeaders
                  , sendBody
                  , sendXmlResponse
                  , sendPartialContentHeaders
                  , HttpError(..)
                  ) where

import Network.HTTP.Base
import Network.HTTP.Headers
import Network.HTTP.Stream
import Network.Socket
import Network.StreamSocket()
import Control.Monad
import Control.Concurrent
import Control.Exception
import Data.Word
import Text.Regex
import Text.Printf
import Network.URI
import RegexExtra

-- Request handlers.
type RequestHandler = 
    Socket -> Request String -> IO ()

-- URL handlers.
type UrlHandler = 
    Socket -> Request String -> [String] -> IO ()

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


sendHeaders :: Socket -> ResponseCode -> String -> [Header] -> IO ()
sendHeaders conn rCode rReason rHeaders = do
  let rsp = Response { rspCode = rCode
                     , rspReason = rReason
                     , rspHeaders = rHeaders
                     , rspBody = "" }
  _ <- writeBlock conn $ show rsp      -- Writes headers only + CRLF.
  return ()

-- Write data to body of request. May be called multiple times after
-- sendHeaders has been called.
sendBody :: Stream s => s -> String -> IO ()
sendBody conn body = do
    _ <- writeBlock conn body
    return ()



myRequestHandler :: [(Regex, UrlHandler)] -> Socket -> Request String -> IO ()
myRequestHandler hs s r =
  case dispatch hs (urlDecode $ uriPath $ rqURI r) of
    Nothing -> sendErrorResponse  s InternalServerError []
    Just (h,gs) -> h s r gs


runHttpServer :: [(Regex, UrlHandler)] -> Word16 -> IO ()
runHttpServer = runHttpServer' . myRequestHandler

{-

   Response generator functions.

-}

sendOkHeaders :: Socket -> [Header] -> Integer -> IO ()
sendOkHeaders conn hs contentLength =
    let h1 = Header HdrContentLength $ printf "%d" contentLength in
    let h2 = Header HdrConnection "close" in
    sendHeaders conn (2,0,0) "OK" (h1 : h2 : hs)

sendPartialContentHeaders :: Socket -> [Header] -> (Integer,Integer) -> Integer -> IO ()
sendPartialContentHeaders conn hs (rLow,rHigh) entitySize =
    let h1 = Header HdrContentLength $ printf "%d" (rHigh-rLow+1) in
    let h2 = Header HdrContentRange $ printf "%d-%d/%d" rLow rHigh entitySize in
    let h3 = Header HdrConnection "close" in
    sendHeaders conn (2,0,6) "Partial Content" (h1 : h2 : h3 : hs)
 


sendXmlResponse :: Socket -> [Header] -> String -> IO ()
sendXmlResponse conn hs xml = do
     sendOkHeaders conn ( Header HdrContentType "text/xml" : hs ) $ toEnum $ length xml
     sendBody conn xml





data HttpError = NotFound
               | InternalServerError
               | NotImplemented
           

sendErrorResponse :: Socket -> HttpError -> [Header] -> IO ()
sendErrorResponse conn e hs =
  sendHeaders conn c r
                  ([ Header HdrContentLength "0"
                   , Header HdrConnection "close"
                   ] ++ hs)
  where
    (c,r) = case e of
              NotFound -> ((4,0,4),"NOT FOUND") 
              InternalServerError -> ((5,0,4), "INTERNAL SERVER ERROR")
              NotImplemented -> ((5,0,1), "NOT IMPLEMENTED")
