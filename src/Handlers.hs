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

module Handlers ( rootDescriptionHandler
                , staticHandler
                , serviceControlHandler
                , contentHandler
                , fallbackHandler
                , State
                ) where

import           Blaze.ByteString.Builder (fromByteString)
import           Control.Exception (bracket)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.CaseInsensitive as CI
import           Data.IORef (IORef, readIORef)
import           Data.List (foldl')
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Network.HTTP.Types (Status, Header, partialContent206, forbidden403, badRequest400, ok200, notFound404)
import           Network.HTTP.Types.Header (hConnection, hContentLength, hContentType)
import           Network.Wai (Application, Request, Response, responseStream, requestBody, requestHeaderRange, responseLBS)
import           Filesystem.Path (FilePath, (</>))
import           Filesystem.Path.CurrentOS (encodeString, fromText)
import qualified Filesystem as FS
import           Prelude hiding (FilePath)
import qualified System.IO as IO
import           Text.Printf (printf)

import           Soap
import           Configuration
import           Service
import           Action
import           MimeType
import           Object
import           HttpExtra

type State = (Configuration, MediaServerConfiguration, ApplicationInformation, [DeviceType], IORef Objects)

{-

   RFC2616 (HTTP/1.1) compliance issues:

     - No conditional range support.
     - Can only handle single ranges.
     - Negative range indexes are not supported.
     - Handling of invalid range specifications is non-compliant.

   It does work well enough for the PS3 though :).

-}

serveStaticFile :: Request -> ByteString -> FilePath -> IO Response
serveStaticFile req mimeType fp = do
  logMessage $ printf "Serving file '%s'..." $ sfp
  -- Do we have a range header?
  case requestHeaderRange req of
    Just value ->
        case parseRangeHeader value of
          Just (l, h) -> serve (Just l, h)
          Nothing -> sendError badRequest400
    Nothing ->
        serve (Nothing, Nothing)
  where
    serve range = do
      fsz <- FS.getSize fp
      return $ serveFile fsz range

    sfp = encodeString fp

    serveFile fsz (l,h) = do
      let l' = maybe 0 id l
      let h' = maybe (fsz-1) id h
      let n = (h' - l' + 1)
      let hdrs = [ hdrContentLength n
                 , (hContentType, mimeType)
                 , hdrContentRange l' h' fsz
                 , hdrAcceptRangesBytes
                 , hdrConnectionClose ]
      responseStream partialContent206 hdrs $ \write _ ->
          bracket
            (IO.openFile sfp IO.ReadMode)
            IO.hClose
            (\hnd -> do
               IO.hSeek hnd IO.AbsoluteSeek l'
               streamBody hnd write $ fromIntegral n)

    bufSize = 32768

    streamBody hnd write = go
      where
        go 0 = return ()
        go n = B.hGet hnd (min bufSize n) >>= \s ->
                  case B.length s of
                    0  -> return ()
                    n' -> write (fromByteString s) >> go (n - n')

-- Handler for the root description.
rootDescriptionHandler :: State -> IO Response
rootDescriptionHandler (c,mc,_,s,_) = do
  logMessage "Got request for root description."
  let xml = generateDescriptionXml c mc s
  return $ responseLBS ok200 [ hdrConnectionClose
                             , hdrContentLength (L.length xml)
                             , xmlContentType
                             ] xml

-- Handle static files.
staticHandler :: Request -> FilePath -> [Text] -> IO Response
staticHandler req root path = do
  logMessage $ "Got request for static content: " ++ (show path)
  if dotDot `elem` path then     -- Reject relative URLs.
    sendError forbidden403
    else
    serveStaticFile req mimeType fp
   where
     fp = foldl' (</>) root (map fromText path)
     mimeType = guessMimeType fp
     dotDot = ".."

-- Handle requests for content.
contentHandler :: Request -> State -> Text -> IO Response
contentHandler req (_,_,_,_,objects_) oid = do
  objects <- readIORef objects_     -- Current snapshot of object tree.
  logMessage $ printf "Got request for CONTENT for objectId=%s" (T.unpack oid)
  -- Serve the file which the object maps to.
  case findByObjectId oid objects of
       Just o ->
         serveStaticFile req mt fp
           where
             od = getObjectData o
             fp = objectFileName od
             mt = objectMimeType od
       Nothing ->
         sendError notFound404

-- Handle requests for device CONTROL urls.
serviceControlHandler :: State -> DeviceType -> Application
serviceControlHandler (c,_,_,_,objects_) deviceType req respond = do
  objects <- readIORef objects_      -- Current snapshot of object tree.
  logMessage $ printf "Got request for CONTROL for service '%s'" $ deviceTypeToString deviceType
  -- Parse the SOAP request
  requestXml <- requestBody req
  logMessage $ "Request: " ++ (show requestXml)
  action <- parseControlSoapXml $ T.unpack $ decodeUtf8 requestXml
  logMessage $ "Action: " ++ (show action)
  -- Deal with the action
  case action of
    Just (ContentDirectoryAction_ cda) ->
      sendXml (generateActionResponseXml c deviceType objects cda) >>= respond
    Just (ConnectionManagerAction_ _) ->
      -- TODO: This should really be implemented as it is required by
      -- the specification. However, the PS3 doesn't seem to use it at
      -- all so I don't have any way to test an implementation anyway.
      sendError notFound404 >>= respond
    Nothing ->
      sendError notFound404 >>= respond

-- Last resort handler.
fallbackHandler :: IO Response
fallbackHandler = return $ responseLBS notFound404 [] ""

-- Send an empty error response.
sendError :: Status -> IO Response
sendError s = return $ responseLBS s [ hdrConnectionClose
                                     , hdrContentLength (0 :: Integer)
                                     ] ""

-- Send generated XML.
sendXml :: L.ByteString -> IO Response
sendXml xml = return $ responseLBS ok200 [ hdrConnectionClose
                                         , hdrContentLength (L.length xml)
                                         , xmlContentType
                                         ] xml

logMessage :: String -> IO ()
logMessage m = putStrLn m

-- Convenience functions for DRY construction of headers.
hdrConnectionClose :: Header
hdrConnectionClose = (hConnection, "close")

hdrAcceptRangesBytes :: Header
hdrAcceptRangesBytes = (CI.mk "accept-ranges", "bytes")

hdrContentRange :: Integer -> Integer -> Integer -> Header
hdrContentRange l h s = (CI.mk "content-range", B8.pack $ printf "%d-%d/%d" l h s)

hdrContentLength :: (Show a, Integral a) => a -> Header
hdrContentLength l = (hContentLength, encodeUtf8 $ T.pack $ show l)

-- XML content type
xmlContentType :: Header
xmlContentType = (hContentType, "text/xml")
