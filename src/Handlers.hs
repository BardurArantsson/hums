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
                , State
                ) where

import           Blaze.ByteString.Builder (fromByteString)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (lift)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.Conduit (ResourceT, Flush(..), ($$))
import qualified Data.Conduit.List as CL
import           Data.Conduit.Binary (sourceFileRange)
import           Data.IORef (IORef, readIORef)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Network.HTTP.Types (Status, Header, status206, statusServerError, statusOK, statusNotFound, headerContentType, headerContentLength, headerConnection)
import           Network.Wai (Application, Request, Response(..), requestBody, requestHeaders, responseLBS)
import           System.FilePath
import           System.IO (withFile, hFileSize, IOMode(..))
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

fileSize :: FilePath -> IO Integer
fileSize fp =
  withFile fp ReadMode $ \h -> hFileSize h

serveStaticFile :: Request -> ByteString -> FilePath -> ResourceT IO Response
serveStaticFile req mimeType fp = do
  logMessage $ printf "Serving file '%s'..." fp
  -- Do we have a range header?
  let ranges = case lookup rangeHeader $ requestHeaders req of
        Just value -> parseRangeHeader $ B8.unpack value
        Nothing    -> [(Nothing, Nothing)] -- whole file
  -- Serve the ranges.
  fsz <- lift $ fileSize fp
  serveFile fsz ranges
  where
    serveFile fsz [(l,h)] = do
      let l' = maybe 0 id l
      let h' = maybe (fsz-1) id h
      let n = (h' - l' + 1)
      let src = fmap (Chunk . fromByteString) $ sourceFileRange fp (Just l') (Just n)
      return $ ResponseSource status206 [ hdrContentLength n
                                        , headerContentType mimeType
                                        , hdrContentRange l' h' fsz
                                        , hdrAcceptRangesBytes
                                        , hdrConnectionClose
                                        ] src

    serveFile _ _ = do
      -- This requires multipart/byteranges, but we don't support that as of yet.
      sendError statusServerError

-- Handler for the root description.
rootDescriptionHandler :: State -> ResourceT IO Response
rootDescriptionHandler (c,mc,ai,s,_) = do
  logMessage "Got request for root description."
  let xml = generateDescriptionXml c mc s
  return $ responseLBS statusOK [ hdrConnectionClose
                                , hdrContentLength (L.length xml)
                                , headerContentType "text/xml"
                                ] xml

-- Handle static files.
staticHandler :: Request -> String -> [Text] -> ResourceT IO Response
staticHandler req root path = do
  logMessage $ "Got request for static content: " ++ (show path)
  if dotDot `elem` path then     -- Reject relative URLs.
    sendError statusServerError
    else
    serveStaticFile req mimeType fp
   where
     fp = foldl (</>) root (map T.unpack path)
     mimeType = guessMimeType fp
     dotDot = ".."

-- Handle requests for content.
contentHandler :: Request -> State -> Text -> ResourceT IO Response
contentHandler req (c,mc,ai,s,objects_) oid = do
  objects <- lift $ readIORef objects_     -- Current snapshot of object tree.
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
         sendError statusNotFound

-- Handle requests for device CONTROL urls.
serviceControlHandler :: State -> DeviceType -> Application
serviceControlHandler (c,mc,ai,s,objects_) deviceType req = do
  objects <- lift $ readIORef objects_      -- Current snapshot of object tree.
  logMessage $ printf "Got request for CONTROL for service '%s'" $ deviceTypeToString deviceType
  -- Parse the SOAP request
  requestXml <- fmap S.concat $ requestBody req $$ CL.consume
  logMessage $ "Request: " ++ (show requestXml)
  action <- lift $ parseControlSoapXml $ T.unpack $ decodeUtf8 requestXml
  logMessage $ "Action: " ++ (show action)
  -- Deal with the action
  case action of
    Just a -> do
      xml_ <- case a of
        ContentDirectoryAction_ cda  -> handleCDA deviceType cda objects
        ConnectionManagerAction_ cma -> handleCMA cma
      return xml_
    Nothing ->
      sendError statusNotFound
  where
    handleCDA st a objects = do
      sendXml $ generateActionResponseXml c st objects a
    handleCMA _ =
      -- TODO: This should really be implemented as it is required by
      -- the specification. However, the PS3 doesn't seem to use it at
      -- all so I don't have any way to test an implementation anyway.
      sendError statusNotFound


-- Last resort handler.
fallbackHandler :: ResourceT IO Response
fallbackHandler = return $ responseLBS statusNotFound [] ""

-- Send an empty error response.
sendError :: Status -> ResourceT IO Response
sendError s = return $ responseLBS s [ hdrConnectionClose
                                     , hdrContentLength (0 :: Integer)
                                     ] ""

-- Send generated XML.
sendXml :: L.ByteString -> ResourceT IO Response
sendXml xml = return $ responseLBS statusOK [ hdrConnectionClose
                                            , hdrContentLength (L.length xml)
                                            , headerContentType "text/xml"
                                            ] xml

logMessage :: String -> ResourceT IO ()
logMessage m = liftIO $ putStrLn m

-- Convenience functions for DRY construction of headers.
hdrConnectionClose :: Header
hdrConnectionClose = headerConnection "close"

hdrAcceptRangesBytes :: Header
hdrAcceptRangesBytes = (CI.mk "accept-ranges", "bytes")

hdrContentRange :: Integer -> Integer -> Integer -> Header
hdrContentRange l h s = (CI.mk "content-range", B8.pack $ printf "%d-%d/%d" l h s)

hdrContentLength :: Integral a => a -> Header
hdrContentLength l = headerContentLength $ encodeUtf8 $ T.pack $ show l

-- Name of the range header.
rangeHeader :: CI ByteString
rangeHeader = CI.mk "range"

