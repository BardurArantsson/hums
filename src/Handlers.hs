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

module Handlers ( rootDescriptionHandler
                , staticHandler
                , serviceControlHandler
                , contentHandler
                , fallbackHandler
                ) where

import Soap
import Configuration
import Network.HTTP.Base
import Network.HTTP.Headers
import Network.StreamSocket()
import Service
import Text.Regex
import Text.Printf
import Action
import System.IO (withFile, hFileSize, IOMode(..))
import MimeType
import Object
import HttpExtra
import System.FilePath
import Data.Maybe (isJust)
import Data.IORef
import HttpMonad
import Control.Monad.Trans.Class (lift)
import Data.ByteString.UTF8 (fromString)
import Control.Monad.IO.Class (MonadIO)

type State = (Configuration, MediaServerConfiguration, ApplicationInformation, [DeviceType], IORef Objects)

{-

   RFC2616 (HTTP/1.1) compliance issues:

     - No conditional range support.
     - Can only handle single ranges.
     - Negative range indexes are not supported.
     - Handling of invalid range specifications is non-compliant.

   It does work well enough for the PS3 though :).

-}


hCanonicalizeRanges :: Integer -> [(Maybe Integer, Maybe Integer)] -> [(Integer,Integer)]
hCanonicalizeRanges fsz ranges =
  map f ranges
  where
    f (lo,hi) = do
      let lo' = case lo of
                  Just x -> x
                  Nothing -> 0
      let hi' = case hi of
                  Just x -> x
                  Nothing -> fsz - 1
      (lo', hi')

fileSize :: FilePath -> IO Integer
fileSize fp =
  withFile fp ReadMode $ \h -> hFileSize h

serveStaticFile :: String -> FilePath -> HttpT IO ()
serveStaticFile mimeType fp = do
  logMessage $ printf "Serving file '%s'..." fp
  -- Do we have a range header?
  hs <- fmap getHeaders getRequest
  let ranges =
          case lookupHeader HdrRange hs of
            Just value -> parseRangeHeader value
            Nothing    -> []       -- Whole file

  -- Set up the common headers.
  addHeader (Header HdrContentType mimeType)
  addHeader (Header (HdrCustom "Accept-Ranges") "bytes")
  addHeader (Header HdrConnection "close")

  -- Serve the ranges.
  fsz <- lift $ fileSize fp
  let ranges' = hCanonicalizeRanges fsz ranges
  serveFile fsz ranges'
  where
    serveFile fsz [] = do
        -- Set the headers for full transfer.
        setResponseCode OK
        setContentLength $ Just fsz
        -- Send the file contents.
        writeFileToBody fp 0 fsz
    serveFile fsz [(rLow,rHigh)] = do
        -- Set the headers for partial content.
        setResponseCode PartialContent
        setContentLength $ Just (rHigh-rLow+1)
        addHeader (Header HdrContentRange $ printf "%d-%d/%d" rLow rHigh fsz)
        -- Send the file contents.
        writeFileToBody fp rLow $ fromInteger $ rHigh - rLow + 1
    serveFile _ _ =
        -- This requires multipart/byteranges, but we don't support that as of yet.
        sendError NotImplemented


-- Regular expressions for avoiding relative URLs. These
-- are overly conservative, but what the heck...
dotDotSlash :: Regex
dotDotSlash = mkRegex "\\.\\./"
slashDotDot :: Regex
slashDotDot = mkRegex "/\\.\\."

-- Handler for the root description.
rootDescriptionHandler :: State -> [String] -> HttpT IO ()
rootDescriptionHandler (c,mc,ai,s,_) gs = do
  logMessage "Got request for root description."
  xml <- lift $ generateDescriptionXml c mc s
  sendXml xml
  logMessage "Sent root description."

-- Handle static files.
staticHandler :: String -> [String] -> HttpT IO ()
staticHandler root gs = do
  logMessage $ "Got request for static content: " ++ show gs
  case gs of
    [p] -> if isJust (matchRegex dotDotSlash p) ||     -- Reject relative URLs.
              isJust (matchRegex slashDotDot p) then
             sendError InternalServerError
           else
             serveStaticFile mimeType fp
           where
             fp = root </> p
             mimeType = guessMimeType fp
    _ ->
      sendError InternalServerError

-- Handle requests for content.
contentHandler :: State -> [String] -> HttpT IO ()
contentHandler (c,mc,ai,s,objects_) gs = do
  objects <- lift $ readIORef objects_     -- Current snapshot of object tree.
  case gs of
    [oid] -> do
          logMessage $ printf "Got request for CONTENT for objectId=%s" oid
          -- Serve the file which the object maps to.
          case findByObjectId oid objects of
               Just o ->
                 serveStaticFile mt fp
                   where
                     od = getObjectData o
                     fp = objectFileName od
                     mt = objectMimeType od
               Nothing ->
                 sendError NotFound
    _ ->
      sendError InternalServerError

-- Handle requests for device CONTROL urls.
serviceControlHandler :: State -> DeviceType -> [String] -> HttpT IO ()
serviceControlHandler (c,mc,ai,s,objects_) deviceType gs = do
  objects <- lift $ readIORef objects_      -- Current snapshot of object tree.
  logMessage $ printf "Got request for CONTROL for service '%s'" $ deviceTypeToString deviceType
  -- Parse the SOAP request
  requestXml <- fmap rqBody getRequest
  logMessage $ printf "Request: %s" requestXml
  action <- lift $ parseControlSoapXml requestXml
  logMessage $ printf "Action: %s" $ show action
  -- Deal with the action
  case action of
    Just a -> do
      xml_ <- case a of
        ContentDirectoryAction_ cda  -> handleCDA deviceType cda objects
        ConnectionManagerAction_ cma -> handleCMA cma
      return ()
    Nothing ->
      sendError NotImplemented
  where
    handleCDA st a objects = do
      xml <- lift $ generateActionResponseXml c st objects a
      logMessage $ printf "Response: %s" $ xml
      sendXml xml
    handleCMA _ =
      -- TODO: This should really be implemented as it is required by
      -- the specification. However, the PS3 doesn't seem to use it at
      -- all so I don't have any way to test an implementation anyway.
      sendError NotImplemented


-- Last resort handler.
fallbackHandler :: HttpT IO ()
fallbackHandler = do
  r <- getRequest
  logMessage $ printf "Fallback handler got request: %s" $ show r
  sendError InternalServerError

-- Send an empty error response.
sendError :: Monad m => HttpResponseCode -> HttpT m ()
sendError c = do
  setResponseCode c
  setContentLength $ Just 0
  addHeader (Header HdrConnection "close")

-- Send generated XML.
sendXml :: (Monad m, MonadIO m, Functor m) => String -> HttpT m ()
sendXml xml = do
  setResponseCode OK
  setContentLength $ Just $ toEnum $ length xml
  addHeader (Header HdrConnection "close")
  addHeader (Header HdrContentType "text/xml")
  writeToBody $ fromString $ xml
