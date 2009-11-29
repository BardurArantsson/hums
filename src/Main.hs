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

import Soap
import Network.Utils
import SimpleServiceDiscoveryProtocol
import Configuration 
import Network.HTTP.Base
import Network.HTTP.Headers
import Network.StreamSocket()
import Control.Concurrent
import HttpServer
import Service
import Text.Regex
import Data.List
import Text.Printf
import Action
import System.IO (withFile, hFileSize, IOMode(..), Handle)
import qualified Data.UUID as U
import qualified Data.UUID.V1 as U1
import MimeType
import Object
import HttpExtra
import Control.Monad.Error
import System.FilePath
import Data.Maybe (fromJust, isJust)
import Data.ConfigFile
import Paths_hums
import Data.IORef
import Network.Socket (Socket)
import Network.Socket.SendFile (sendFile', sendFileMode)

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

type State = (Configuration, MediaServerConfiguration, ApplicationInformation, [DeviceType], IORef Objects)


rootDescriptionHandler :: State -> Socket -> Request String -> [String] -> IO ()
rootDescriptionHandler (c,mc,ai,s,_) conn r gs = do
  putStrLn "Got request for root description."
  xml <- generateDescriptionXml c mc s
  putStrLn "Generated root description."
  sendXmlResponse conn (getExtraHeaders ai) xml
  putStrLn "Send root description."

hCopyBytes :: Handle -> Socket -> Integer -> Integer -> IO ()
hCopyBytes src dst ofs len = do
  putStrLn $ printf "Sending %d bytes..." len
  sendFile' dst src ofs len

-- Copy a set of ranges between two handles.
hCopyRanges :: Handle -> Socket -> [(Integer,Integer)] -> IO ()
hCopyRanges hnd conn ranges =
  mapM_ copyRange ranges
  where
    copyRange (lo, hi) = hCopyBytes hnd conn lo $ fromInteger $ hi - lo + 1

{-

   RFC2616 (HTTP/1.1) compliance issues:

     - No conditional range support.
     - Can only handle single ranges.
     - Negative range indexes are not supported.
     - Handling of invalid range specifications is non-compliant.

   It does work well enough for the PS3 though :).

-}


hCanonicalizeRanges :: Handle -> [(Maybe Integer, Maybe Integer)] -> IO [(Integer,Integer)]
hCanonicalizeRanges hnd ranges =
  mapM f ranges
  where 
    f (lo,hi) = do
      let lo' = case lo of 
                  Just x -> x
                  Nothing -> 0
      hi' <- case hi of 
               Just x -> return x
               Nothing -> do
                       l <- hFileSize hnd
                       return $ l - 1
      return (lo', hi')

serveStaticFile :: Socket -> [Header] -> String -> FilePath -> IO ()
serveStaticFile conn hs mimeType fp = do
  putStrLn $ printf "Serving file '%s'..." fp

  -- Do we have a range header?
  let ranges = 
          case lookupHeader HdrRange hs of
            Just value -> parseRangeHeader value
            Nothing    -> []       -- Whole file

  -- Serve the ranges.
  withFile fp ReadMode $ \h ->
      hCanonicalizeRanges h ranges >>= serveFile h
  where 
    ohs = [ Header HdrContentType mimeType ]
    serveFile h [] = do
        -- No range given (or all ranges were invalid), so we handle regularly.
        len <- hFileSize h
        sendOkHeaders conn ohs $ fromInteger len
        hCopyBytes h conn 0 len
    serveFile h [r] = do
        fs <- fmap fromInteger $ hFileSize h
        -- Send headers
        sendPartialContentHeaders conn ohs r fs
        -- Copy data from ranges into body.
        hCopyRanges h conn [r]
    serveFile h _ =
        -- This requires multipart/byteranges, but we don't support that
        -- as of yet.
        error "Cannot handle multiple ranges in a single request."


-- Regular expressions for avoiding relative URLs. These
-- are overly conservative, but what the heck...
dotDotSlash :: Regex
dotDotSlash = mkRegex "\\.\\./"
slashDotDot :: Regex
slashDotDot = mkRegex "/\\.\\." 

-- Handle static files.
staticHandler :: String -> Socket -> Request String -> [String] -> IO ()
staticHandler root conn req gs = do
    putStrLn $ "Got request for static content: " ++ show gs
    case gs of
      [p] -> if isJust (matchRegex dotDotSlash p) ||     -- Reject relative URLs.
                isJust (matchRegex slashDotDot p) then
                 sendErrorResponse conn InternalServerError []
               else
                   serveStaticFile conn (getHeaders req) mimeType fp
             where 
               fp = root </> p
               mimeType = guessMimeType fp
      _ -> sendErrorResponse conn InternalServerError []
    


-- TODO: Should we generate a DATE header?
getExtraHeaders :: ApplicationInformation -> [ Header ]
getExtraHeaders ai = [ Header (HdrCustom "contentFeatures.dlna.org") ""
                     , Header (HdrCustom "EXT") ""
                     , Header (HdrCustom "Server") (getServerHeaderValue ai)
                     , Header (HdrCustom "Accept-Ranges") "bytes"
                     ]

contentHandler :: State -> Socket -> Request String -> [String] -> IO ()
contentHandler (c,mc,ai,s,objects_) conn r gs = do
  objects <- readIORef objects_     -- Current snapshot of object tree.
  case gs of
    [oid] -> do
            putStrLn $ printf "Got request for CONTENT for objectId=%s" oid
            -- Serve the file which the object maps to.
            -- FIXME: Return 404 error code if file has disappeared.
            case findByObjectId oid objects of
                 Just o -> serveStaticFile conn (getHeaders r) mt fp
                           where 
                             od = getObjectData o
                             fp = objectFileName od
                             mt = objectMimeType od
                 Nothing -> sendErrorResponse conn NotFound []

    _ -> sendErrorResponse conn InternalServerError []



serviceControlHandler :: State -> Socket -> Request String -> [String] -> IO ()
serviceControlHandler (c,mc,ai,s,objects_) conn r gs = do
  objects <- readIORef objects_      -- Current snapshot of object tree.
  case gs of
    [sn] -> do
            putStrLn $ printf "Got request for CONTROL for service '%s'" sn
            case stringToDeviceType sn of
              Just dt -> do
                -- Parse the SOAP request
                let requestXml = rqBody r
                action <- parseControlSoapXml requestXml
                -- Deal with the action
                case action of
                  Just a -> do
                    x <- case a of
                           ContentDirectoryAction_ cda -> handleCDA dt cda objects
                           ConnectionManagerAction_ cma -> handleCMA cma
                    putStrLn $ printf "Response:\n\n%s\n\n" x
                    sendXmlResponse conn (getExtraHeaders ai) x
                  Nothing -> 
                      sendErrorResponse conn NotImplemented []
              Nothing -> do
                  putStrLn $ printf "Asked about unknown service '%s'" sn
                  sendErrorResponse conn NotImplemented []
    _ ->
      -- Mapped to our URL space, but not parseable? 
      sendErrorResponse conn NotFound []
  where
    handleCDA st a objects =
        generateActionResponseXml c st objects a
    handleCMA _ =
      -- TODO: This should really be implemented as it is required by
      -- the specification. However, the PS3 doesn't seem to use it at
      -- all so I don't have any way to test an implementation anyway.
      error "Not implemented"



fallbackHandler :: Socket -> Request String -> [String] -> IO ()
fallbackHandler conn r gs = do
    putStrLn "Fallback handler got request:"
    print r
    sendErrorResponse conn InternalServerError []


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

      -- Extra information for debugging:
      putStrLn $ printf "SendFile mode: %s" sendFileMode

      -- Build URL dispatch table
      let dispatchTable = 
              [ (mkRegex "^/description\\.xml$", rootDescriptionHandler st)
              , (mkRegex "^/static/(.*)$", staticHandler $ dataDirectory </> "www")
              , (mkRegex "^/dynamic/services/([^/]+)/control/?$", serviceControlHandler st)
              , (mkRegex "^/content/([0-9a-f,]+)$", contentHandler st)
              , (mkRegex "(.*)", fallbackHandler)
              ]

      -- Start serving.
      putStrLn "Establishing HTTP server..."
      forkIO $ runHttpServer dispatchTable $ httpServerPort c
      putStrLn "Done."
      
      -- Start broadcasting alive messages.
      putStrLn "Establishing notification broadcaster..."
      forkIO $ sendNotifyForever appInfo c mc

      -- Start scanning files/directories in the background.
      putStrLn "Establishing background scanner..."
      forkIO $ periodicUpdate defaultObjects scanOnce'

      -- Wait for all threads to terminate.
      interact id

      return ()
