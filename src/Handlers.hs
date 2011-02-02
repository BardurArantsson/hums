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

import Soap
import Configuration
import Service
import Text.Printf
import Action
import Blaze.ByteString.Builder (insertByteString)
import System.IO (withFile, hFileSize, IOMode(..))
import MimeType
import Object
import HttpExtra
import System.FilePath
import Data.ByteString (ByteString, isInfixOf)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B8
import Data.IORef
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.Wai
import Data.Enumerator (Iteratee, ($$))
import Data.Enumerator.Binary (enumFileRange)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

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

serveStaticFile :: Request -> ByteString -> FilePath -> Iteratee ByteString IO Response
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
      return $ ResponseEnumerator $ \f ->
        E.run_ $ (E.joinE
                  (enumFileRange fp l h)
                  (EL.map insertByteString)) $$ f status206
          [ hdrContentLength n
          , hdrContentType mimeType
          , hdrContentRange l' h' fsz
          , hdrAcceptRangesBytes
          , hdrConnectionClose
          ]
    serveFile _ _ = do
      -- This requires multipart/byteranges, but we don't support that as of yet.
      sendError statusServerError

-- Regular expressions for avoiding relative URLs. These
-- are overly conservative, but what the heck...
dotDotSlash :: ByteString
dotDotSlash = "../"
slashDotDot :: ByteString
slashDotDot = "/.."

-- Handler for the root description.
rootDescriptionHandler :: State -> Iteratee ByteString IO Response
rootDescriptionHandler (c,mc,ai,s,_) = do
  logMessage "Got request for root description."
  let xml = generateDescriptionXml c mc s
  return $ responseLBS statusOK [ hdrConnectionClose
                                , hdrContentLength (L.length xml)
                                , hdrContentType "text/xml"
                                ] xml

-- Handle static files.
staticHandler :: Request -> String -> ByteString -> Iteratee ByteString IO Response
staticHandler req root p = do
  logMessage $ "Got request for static content: " ++ (show p)
  if dotDotSlash `isInfixOf` p ||     -- Reject relative URLs.
     slashDotDot `isInfixOf` p then
    sendError statusServerError
    else
    serveStaticFile req mimeType fp
   where
     fp = root </> (T.unpack $ decodeUtf8 p)
     mimeType = guessMimeType fp

-- Handle requests for content.
contentHandler :: Request -> State -> ByteString -> Iteratee ByteString IO Response
contentHandler req (c,mc,ai,s,objects_) oid = do
  objects <- lift $ readIORef objects_     -- Current snapshot of object tree.
  logMessage $ printf "Got request for CONTENT for objectId=%s" (B8.unpack oid)
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
serviceControlHandler :: State -> DeviceType -> ByteString -> Iteratee ByteString IO Response
serviceControlHandler (c,mc,ai,s,objects_) deviceType _ = do
  objects <- lift $ readIORef objects_      -- Current snapshot of object tree.
  logMessage $ printf "Got request for CONTROL for service '%s'" $ deviceTypeToString deviceType
  -- Parse the SOAP request
  requestXml <- fmap S.concat EL.consume
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
fallbackHandler :: Iteratee ByteString IO Response
fallbackHandler = do
  return $ responseLBS statusNotFound [] ""

-- Send an empty error response.
sendError :: Monad m => Status -> Iteratee ByteString m Response
sendError s = return $
              responseLBS s [ hdrConnectionClose
                            , hdrContentLength (0 :: Integer)
                            ] ""

-- Send generated XML.
sendXml :: (MonadIO m, Functor m) => L.ByteString -> Iteratee ByteString m Response
sendXml xml = return $
  responseLBS statusOK [ hdrConnectionClose
                       , hdrContentLength (L.length xml)
                       , hdrContentType "text/xml"
                       ] xml

logMessage :: (MonadIO m, Functor m) => String -> Iteratee a m ()
logMessage m = do
  liftIO $ putStrLn m

-- Convenience functions for DRY construction of headers.
hdrContentType :: ByteString -> (ResponseHeader, ByteString)
hdrContentType t = ("Content-Type", t)

hdrContentLength :: Integral a => a -> (ResponseHeader, ByteString)
hdrContentLength l = ("Content-Length", encodeUtf8 $ T.pack $ show l)

hdrConnectionClose :: (ResponseHeader, ByteString)
hdrConnectionClose = ("Connection", "close")

hdrAcceptRangesBytes :: (ResponseHeader, ByteString)
hdrAcceptRangesBytes = (mkCIByteString "accept-ranges", "bytes")

hdrContentRange :: Integer -> Integer -> Integer -> (ResponseHeader, ByteString)
hdrContentRange l h s = (mkCIByteString "content-range", B8.pack $ printf "%d-%d/%d" l h s)

-- Name of the range header.
rangeHeader :: CIByteString
rangeHeader = mkCIByteString "range"

