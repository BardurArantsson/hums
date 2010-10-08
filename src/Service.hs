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

module Service ( generateDescriptionXml
               , generateActionResponseXml
               , DeviceType(..)
               , deviceTypeToString
               ) where
    
import Text.XML.HXT.Arrow
import Text.Printf
import Configuration
import Action
import Data.Maybe (mapMaybe)
import Object
import Data.List.Utils
import Data.Bits
import Data.Int
import MimeType
import URIExtra

defaultEncodingAttributes :: Attributes
defaultEncodingAttributes = [ (a_output_encoding, utf8) ]

myQuote :: String -> String
myQuote =        -- TODO: There *must* be a better way to achieve our quoting needs.
    concatMap f
    where
      f '<' = "&lt;"
      f '>' = "&gt;"
      f c = [c]



sanitizeXmlChars :: String -> String
sanitizeXmlChars = 
    map f
    where
      f '<' = '_'
      f '>' = '_'
      f '&' = '_'
      f  c  = c



optSelem :: ArrowXml a => String -> Maybe String -> a n XmlTree
optSelem n Nothing = cmt $ printf " %s omitted " n
optSelem n (Just x) = selem n [txt x]

data DeviceType = MediaServer
                | ContentDirectoryDevice
                | ConnectionManagerDevice
                  
deviceTypeToString :: DeviceType -> String
deviceTypeToString ContentDirectoryDevice = "ContentDirectory"
deviceTypeToString ConnectionManagerDevice = "ConnectionManager"
deviceTypeToString MediaServer = "MediaServer"

serviceNs :: ArrowXml a => String -> DeviceType -> a XmlTree XmlTree
serviceNs prefix st = 
    sattr an av
    where 
      an = printf "xmlns:%s" prefix
      av = printf "urn:schemas-upnp-org:service:%s:1" $ deviceTypeToString st

serviceNs' :: DeviceType -> String
serviceNs' = printf "urn:schemas-upnp-org:service:%s:1" . deviceTypeToString

-- Generate the icon list.
generateIconList :: ArrowXml a => Bool -> a XmlTree XmlTree
generateIconList False = cmt " omitted device icon list "
generateIconList True = 
    selem "iconList" 
              [ selem "icon"
                [ selem "mimetype" [ txt $ guessMimeType imageUrl ]
                , selem "width"    [ txt "240" ] 
                , selem "height"   [ txt "240" ]
                , selem "url"      [ txt imageUrl ]
                ]
              ]
    where
      imageUrl = "/static/images/hums.jpg"

generateServiceList :: ArrowXml a => [DeviceType] -> a XmlTree XmlTree
generateServiceList services = 
    selem "serviceList" $ map generateService services
    where
      generateService service =
          selem "service" 
                    [ selem "serviceType" [txt $ serviceNs' service]
                    , selem "serviceId" [txt $ printf "urn:upnp-org:serviceId:%s" dt]
                    , selem "SCPDURL" [txt $ printf "/static/services/%s/description.xml" dt]
                    , selem "controlURL" [txt $ printf "/dynamic/services/%s/control/" dt]
	            , selem "eventSubURL" [txt $ printf "/dynamic/services/%s/event/" dt]
                    ]
          where dt = deviceTypeToString service

generateDescription :: ArrowXml a => Configuration -> MediaServerConfiguration -> [DeviceType] -> a XmlTree XmlTree
generateDescription c mc services = 
    root []
     [ mkelem "root" [sattr "xmlns" "urn:schemas-upnp-org:device-1-0"]
       [ selem "specVersion"
         [ selem "major" [ txt "1" ]
         , selem "minor" [ txt "0" ]
         ]
       , selem "URLBase" [ txt $ show $ httpServerBase c ]
       , selem "device"
         [ selem "UDN" [ txt $ printf "uuid:%s" $ uuid mc ]
         , selem "friendlyName" [ txt $ friendlyName mc ]
         , selem "manufacturer" [ txt $ manufacturer mc ]
         , selem "manufacturerURL" [ txt $ manufacturerUrl mc ]
         , optSelem "modelDescription" $ modelDescription mc
         , selem "modelName" [ txt $ modelName mc ]
         , selem "modelNumber" [ txt $ modelNumber mc ]
         , selem "modelURL" [ txt $ modelUrl mc ]
         , optSelem "serialNumber" $ serialNumber mc
         , selem "deviceType" [ txt $ printf "urn:schemas-upnp-org:device:%s:1" deviceType ]
         , optSelem "UPC" $ upc mc
--         , optSelemNs "dlna:X_DNLADOC" [sattr "xmlns" "urn:schemas-dlna-org:device-1-0"] $ dlna
         , generateIconList $ enableDeviceIcon c
         , generateServiceList services
         , selem "presentationURL" [ txt presentationUrl ]
         ]
       ]
     ]
    where
      deviceType = deviceTypeToString MediaServer
--      dlna = if useDlna mc then (Just "DMS-1.00") else Nothing
      presentationUrl = "index.html"

-- Transform an XmlTree to a string.
generateXml :: Attributes -> IOSLA (XIOState ()) XmlTree XmlTree -> IO String
generateXml as a = do
  xml <- runX (a >>> writeDocumentToString (addEntries as defaultEncodingAttributes))
  return $ concat xml

generateDescriptionXml :: Configuration -> MediaServerConfiguration -> [DeviceType] -> IO String
generateDescriptionXml c mc =
  generateXml [] . generateDescription c mc


generateResponseXml :: [IOSLA (XIOState ()) XmlTree XmlTree] -> IO String
generateResponseXml =
  generateXml [(a_output_xml,v_0)] . generateSoapEnvelope

generateBrowseResponseXml :: Configuration -> DeviceType -> Objects -> BrowseAction -> IO String

generateBrowseResponseXml cfg st os (BrowseMetadata bps) = do
  didlXml <- fmap myQuote $ generateXml [] didl
  let body = [ mkelem "u:BrowseResponse" [ serviceNs "u" st ]
               [ selem "Result" [ txt didlXml ]
               , selem "NumberReturned" [ txt "1" ]  -- CD/§2.7.4.2
               , selem "TotalMatches" [ txt "1" ]    -- CD/§2.7.4.2
               , selem "UpdateID" [ txt $ printf "%d" $ systemUpdateId os ]
               ]
             ]
  generateResponseXml body
  where 
    didl = mkDidl [generateObjectElement cfg os (oid,o)]
    oid = objectId bps
    o = findExistingByObjectId oid os -- TODO: Might handle non-existing objects better.


generateBrowseResponseXml cfg st os (BrowseDirectChildren bps) = do
  didlXml <- fmap myQuote $ generateXml [] didl
  let body = [ mkelem "u:BrowseResponse" [ serviceNs "u" st ]
               [ selem "Result" [ txt didlXml ]
               , selem "NumberReturned" [ txt numberReturned ]
               , selem "TotalMatches" [ txt totalMatches ]
               , selem "UpdateID" [ txt $ printf "%d" $ systemUpdateId os ]
               ]
             ]
  generateResponseXml body
  where 
    oid = objectId bps
    si = startingIndex bps
    rc = requestedCount bps
    totalMatches = show $ getNumberOfChildren os oid     --  CD/§2.7.4.2
    numberReturned = show $ length chosenChildren
    slicer = if rc<=0 then id else slice si rc -- CD/§2.7.4.2
    chosenChildren = slicer $ Object.getChildren os oid
    didl = mkDidl $ map (generateObjectElement cfg os) chosenChildren



generateActionResponseXml :: Configuration -> DeviceType -> Objects -> ContentDirectoryAction -> IO String

generateActionResponseXml _ st os ContentDirectoryGetSystemUpdateId =
  generateXml [] $ generateSoapEnvelope body
  where
    body = [ mkelem "u:GetSystemUpdateIDResponse" [ serviceNs "u" st ]
             [ selem "Id" [ txt $ show $ systemUpdateId os ] ] ]

generateActionResponseXml cfg st os (ContentDirectoryBrowse ba) =
  generateBrowseResponseXml cfg st os ba

generateActionResponseXml _ st _ ContentDirectoryGetSearchCapabilities =
  generateXml [] $ generateSoapEnvelope body
  where 
    body = [ mkelem "u:GetSearchCapabilitiesResponse" [ serviceNs "u" st ]
             [ selem "SearchCaps" [ txt "" ] ]   -- No search capabilities (CD/§2.5.18)
           ]

generateActionResponseXml _ st _ ContentDirectoryGetSortCapabilities =
  generateXml [] $ generateSoapEnvelope body
  where 
    body = [ mkelem "u:GetSortCapabilitiesResponse" [ serviceNs "u" st ]
             [ selem "SortCaps" [ txt "" ] ]   -- No sorting capabilities (CD/§2.5.19)
           ]

generateSoapEnvelope :: ArrowXml a => [a XmlTree XmlTree] -> a XmlTree XmlTree
generateSoapEnvelope b = 
    root []
      [ mkelem "s:Envelope" [ soapNs, soapEncodingStyle ]
        [ selem "s:Body" b ]
      ]
    where
      soapNs = sattr "xmlns:s" $ printf "%s/envelope/" urlPrefix
      soapEncodingStyle = sattr "s:encodingStyle" $ printf "%s/encoding/" urlPrefix
      urlPrefix = "http://schemas.xmlsoap.org/soap"


generateObjectElement :: ArrowXml a => Configuration -> Objects -> (ObjectId, Object) -> a XmlTree XmlTree
generateObjectElement cfg objects (oid, o) = 
    mkelem en (as ++ eas)
     ([ selem "dc:title" [ txt $ sanitizeXmlChars $ objectTitle od ]
      , selem "upnp:class" [ txt $ getObjectClassName o ]
      ] ++ ee)
    where
      od = getObjectData o
      en = getObjectElementName o
      ee = generateExtraElements cfg (oid,o)
      as = [ sattr "id" oid 
           , sattr "parentID" $ objectParentId od
           ]
      eas = generateExtraAttributes objects (oid,o)


-- Generate any attributes required for any given object.
-- (Apart from the attributes of the 'object' class itself.)
generateExtraAttributes :: ArrowXml a => Objects -> (ObjectId, Object) -> [a XmlTree XmlTree]
generateExtraAttributes objects (oid, (Container,_)) = genContainerAttributes objects oid
generateExtraAttributes objects (oid, (ContainerStorageFolder,_)) = genContainerAttributes objects oid
generateExtraAttributes _ (_, (ItemMusicTrack,_)) = genItemAttributes
generateExtraAttributes _ (_, (ItemVideoMovie,_)) = genItemAttributes

-- Generate content URL
generateContentUrl :: ArrowXml a => Configuration -> ObjectId -> a XmlTree XmlTree
generateContentUrl cfg oid = 
    txt $ show $ mkURI ["content", oid] $ httpServerBase cfg

-- Generate any extra elements for any given object.
generateExtraElements :: ArrowXml a => Configuration -> (ObjectId, Object) -> [a XmlTree XmlTree]
generateExtraElements _ (oid, (Container,_)) = []
generateExtraElements _ (oid, (ContainerStorageFolder,_)) = []
generateExtraElements cfg (oid, (ItemMusicTrack,d)) = 
    [ mkelem "res" [ sattr "protocolInfo" protocolInfo
                   , sattr "size" $ printf "%d" $ objectFileSize d ] -- TODO: should be disabled by Transcoding flag!
      [ generateContentUrl cfg oid ]
    ]
    where
      mimeType = objectMimeType d
      protocolInfo = generateProtocolInfo cfg False mimeType Nothing  -- TODO: profileId

generateExtraElements cfg (oid, (ItemVideoMovie,d)) =
    [ mkelem "res" [ sattr "protocolInfo" protocolInfo 
                   , sattr "size" $ printf "%d" $ objectFileSize d ] -- TODO: should be disabled by Transcoding flag!
      [ generateContentUrl cfg oid ]
    ]
    where
      mimeType = objectMimeType d
      protocolInfo = generateProtocolInfo cfg False mimeType Nothing  -- TODO: profileId

mapMaybe1 :: (a -> b) -> Maybe a -> Maybe b
mapMaybe1 f Nothing  = Nothing
mapMaybe1 f (Just a) = Just $ f a

generateProtocolInfo :: Configuration -> Bool -> String -> Maybe String -> String
generateProtocolInfo cfg transcode mimeType profileId =
    protocolPrefix ++ protocolSuffix
    where 
      playSpeed = 1 :: Int           -- DLNA play speed: Normal
      conversionFlags =              -- DLNA conversion flags
          if transcode then 1 else 0 :: Int
      operationsParameter =          -- DLNA operations parameter
          if transcode then 0 :: Int -- Don't support bytes ranges, nor time seek ranges.
            else 1                   -- Support byte ranges, but not time seek ranges.
      flags =
               _DLNA_ORG_FLAG_STREAMING_TRANSFER_MODE .|.
               _DLNA_ORG_FLAG_BACKGROUND_TRANSFER_MODE .|.
               _DLNA_ORG_FLAG_CONNECTION_STALL .|.
               _DLNA_ORG_FLAG_DLNA_V15 .|.
               if transcode then 0 else _DLNA_ORG_FLAG_BYTE_BASED_SEEK
      fields =
                [ ("DLNA.ORG_PS", Just $ printf "%d" playSpeed)
                , ("DLNA.ORG_CI", Just $ printf "%d" conversionFlags)
                , ("DLNA.ORG_OP", Just $ printf "%02x" operationsParameter)
                , ("DLNA.ORG_PN", dlnaProfileName cfg)
                , ("DLNA.ORG_FLAGS" , Just $ printf "%08x%024x" flags (0 :: Int32) ) ]
      protocolPrefix = "http-get:*:" ++ mimeType ++ ":"
      protocolSuffix = 
          if useDlna cfg then
              join ";" $ mapMaybe (\(n,v) -> mapMaybe1 (printf "%s=%s" n) v) fields
          else 
              "*"
      -- Protocol constants.
      _DLNA_ORG_FLAG_SENDER_PACED              = bit 31 :: Int32
      _DLNA_ORG_FLAG_TIME_BASED_SEEK           = bit 30 :: Int32
      _DLNA_ORG_FLAG_BYTE_BASED_SEEK           = bit 29 :: Int32
      _DLNA_ORG_FLAG_PLAY_CONTAINER            = bit 28 :: Int32
      _DLNA_ORG_FLAG_S0_INCREASE               = bit 27 :: Int32
      _DLNA_ORG_FLAG_SN_INCREASE               = bit 26 :: Int32
      _DLNA_ORG_FLAG_RTSP_PAUSE                = bit 25 :: Int32
      _DLNA_ORG_FLAG_STREAMING_TRANSFER_MODE   = bit 24 :: Int32
      _DLNA_ORG_FLAG_INTERACTIVE_TRANSFER_MODE = bit 23 :: Int32
      _DLNA_ORG_FLAG_BACKGROUND_TRANSFER_MODE  = bit 22 :: Int32
      _DLNA_ORG_FLAG_CONNECTION_STALL          = bit 21 :: Int32
      _DLNA_ORG_FLAG_DLNA_V15                  = bit 20 :: Int32


-- Gnerate extra attributes for containers.
genContainerAttributes :: ArrowXml a => Objects -> ObjectId -> [a XmlTree XmlTree]
genContainerAttributes objects oid =
    [ sattr "searchable" "0"
    , sattr "restricted" "0"
    , sattr "childCount" $ show $ getNumberOfChildren objects oid
    ]

-- Generate extra attributes for items.
genItemAttributes :: ArrowXml a => [a XmlTree XmlTree]
genItemAttributes = []

-- Create the DIDL result object.
mkDidl :: ArrowXml a => [a XmlTree XmlTree] -> a XmlTree XmlTree
mkDidl es =
    selem "dummy"    -- Not using 'root' means we avoid the XML declaration
              [ mkelem "DIDL-Lite" [ sattr "xmlns:dc" dcNs
                                   , sattr "xmlns:upnp" upnpNs
                                   , sattr "xmlns" ns ]
                es ]
    where
      dcNs = "http://purl.org/dc/elements/1.1/"
      upnpNs = "urn:schemas-upnp-org:metadata-1-0/upnp/"
      ns = "urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/"


-- Slice out a portion of a list.
slice :: Int -> Int -> [a] -> [a]
slice i n = take n . drop i
