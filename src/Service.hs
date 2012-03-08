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

import           Data.Bits
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Int
import           Data.List.Utils
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Text.Printf
import           Text.XML.HaXml.Types
import           Text.XML.HaXml.ByteStringPP

import Configuration
import Action
import Object
import MimeType
import URIExtra

myQuote :: Text -> Text
myQuote =        -- TODO: There *must* be a better way to achieve our quoting needs.
    TL.concatMap f
    where
      f '<' = TL.pack "&lt;"
      f '>' = TL.pack "&gt;"
      f c   = TL.singleton c

sanitizeXmlChars :: String -> String
sanitizeXmlChars = map f
    where
      f '<' = '_'
      f '>' = '_'
      f '&' = '_'
      f  c  = c

--
-- Helpers for constructing XML.
--
comment :: String -> Content ()
comment c = CMisc (Comment c) ()

simpleElement :: String -> [Content ()] -> Content ()
simpleElement n c = CElem (Elem (N n) [] c) ()

textElement :: String -> String -> Content ()
textElement n t = optTextElement n $ Just t

emptyElement :: Name -> Content ()
emptyElement n = elementToContent $ Elem (N n) [] []

text :: String -> Content ()
text t = CString False t ()

optTextElement :: Name -> Maybe String -> Content ()
optTextElement n Nothing = comment $ printf " %s omitted" n
optTextElement n (Just t) = elementToContent $ Elem (N n) [] [text t]

attribute :: String -> String -> Attribute
attribute n v = (N n, AttValue [Left v])

mkDocument :: Element () -> Document ()
mkDocument rootElement =
  (Document _prolog emptyST rootElement [])
    where
      _prolog = Prolog xmlDecl [] Nothing []
      xmlDecl = Just $ XMLDecl xmlVers encDecl Nothing
      encDecl = Just $ EncodingDecl "utf-8"
      xmlVers = "1.0"

elementToContent :: Element () -> Content ()
elementToContent e = CElem e ()

-- Device types.
data DeviceType = MediaServer
                | ContentDirectoryDevice
                | ConnectionManagerDevice

deviceTypeToString :: DeviceType -> String
deviceTypeToString ContentDirectoryDevice = "ContentDirectory"
deviceTypeToString ConnectionManagerDevice = "ConnectionManager"
deviceTypeToString MediaServer = "MediaServer"

serviceNs :: String -> DeviceType -> Attribute
serviceNs prefix st = attribute an av
  where
    an = printf "xmlns:%s" prefix
    av = printf "urn:schemas-upnp-org:service:%s:1" $ deviceTypeToString st

serviceNs' :: DeviceType -> String
serviceNs' = printf "urn:schemas-upnp-org:service:%s:1" . deviceTypeToString

-- Generate the icon list.
generateIconList :: Bool -> Content ()
generateIconList False = comment " omitted device icon list "
generateIconList True =
  (simpleElement "iconList"
   [ simpleElement "icon"
     [ textElement "mimetype" $ B8.unpack $ guessMimeType imageUrl
     , textElement "width"      "240"
     , textElement "height"     "240"
     , textElement "url"        imageUrl
     ]
   ])
    where
      imageUrl = "/static/images/hums.jpg"

generateServiceList :: [DeviceType] -> Content ()
generateServiceList services =
  simpleElement "serviceList" $ map generateService services
    where
      generateService service =
        ( simpleElement "service"
          [ textElement "serviceType" $ serviceNs' service
          , textElement "serviceId"   $ printf "urn:upnp-org:serviceId:%s" dt
          , textElement "SCPDURL"     $ printf "/static/services/%s/description.xml" dt
          , textElement "controlURL"  $ printf "/dynamic/services/%s/control/" dt
          , textElement "eventSubURL" $ printf "/dynamic/services/%s/event/" dt
          ] )
          where dt = deviceTypeToString service

generateDescription :: Configuration -> MediaServerConfiguration -> [DeviceType] -> Document ()
generateDescription c mc services =
  (mkDocument
   (Elem (N "root") [attribute "xmlns" "urn:schemas-upnp-org:device-1-0"]
    [ simpleElement "specVersion"
      [ textElement "major" "1"
      , textElement "minor" "0"
      ]
    , textElement "URLBase" $ show $ httpServerBase c
    , simpleElement "device"
      [ textElement "UDN" $ printf "uuid:%s" $ uuid mc
      , textElement "friendlyName" $ friendlyName mc
      , textElement "manufacturer" $ manufacturer mc
      , textElement "manufacturerURL" $ manufacturerUrl mc
      , optTextElement "modelDescription" $ modelDescription mc
      , textElement "modelName" $ modelName mc
      , textElement "modelNumber" $ modelNumber mc
      , textElement "modelURL" $ modelUrl mc
      , optTextElement "serialNumber" $ serialNumber mc
      , textElement "deviceType" $ printf "urn:schemas-upnp-org:device:%s:1" deviceType
      , optTextElement "UPC" $ upc mc
--    , optSelemNs "dlna:X_DNLADOC" [sattr "xmlns" "urn:schemas-dlna-org:device-1-0"] $ dlna
      , generateIconList $ enableDeviceIcon c
      , generateServiceList services
      , textElement "presentationURL" "index.html"
      ]
    ]))
    where
      deviceType = deviceTypeToString MediaServer
--      dlna = if useDlna mc then (Just "DMS-1.00") else Nothing

mkBrowseResponse :: Integral a => Configuration -> DeviceType -> Objects -> a -> a -> Element () -> Element ()
mkBrowseResponse cfg st os numberReturned totalMatches didl =
  ( Elem (N "u:BrowseResponse") [ serviceNs "u" st ]
    [ textElement "Result" didlXml
    , textElement "NumberReturned" $ show numberReturned   -- CD/§2.7.4.2
    , textElement "TotalMatches"   $ show totalMatches     -- CD/§2.7.4.2
    , textElement "UpdateID" $ printf "%d" $ systemUpdateId os
    ] )
  where
    didlXml = TL.unpack $ myQuote $ decodeUtf8 $ element $ didl

generateBrowseResponse :: Configuration -> DeviceType -> Objects -> BrowseAction -> Element ()
generateBrowseResponse cfg st os (BrowseMetadata bps) =
  mkBrowseResponse cfg st os n n didl
    where
      n = 1 :: Int
      didl = mkDidl [generateObjectElement cfg os (oid,o)]
      oid = objectId bps
      o = findExistingByObjectId oid os -- TODO: Might handle non-existing objects better.

generateBrowseResponse cfg st os (BrowseDirectChildren bps) =
  mkBrowseResponse cfg st os numberReturned totalMatches didl
  where
    oid = objectId bps
    si = startingIndex bps
    rc = requestedCount bps
    totalMatches = getNumberOfChildren os oid     --  CD/§2.7.4.2
    numberReturned = length chosenChildren
    slicer = if rc<=0 then id else slice si rc -- CD/§2.7.4.2
    chosenChildren = slicer $ Object.getChildren os oid
    didl = mkDidl $ map (generateObjectElement cfg os) chosenChildren



generateDescriptionXml :: Configuration -> MediaServerConfiguration -> [DeviceType] -> ByteString
generateDescriptionXml c mc = document . generateDescription c mc

generateResponseXml :: Element () -> ByteString
generateResponseXml d = document $ generateSoapEnvelope d

generateBrowseResponseXml :: Configuration -> DeviceType -> Objects -> BrowseAction -> ByteString
generateBrowseResponseXml cfg st os action = generateResponseXml $ generateBrowseResponse cfg st os action

generateActionResponseXml :: Configuration -> DeviceType -> Objects -> ContentDirectoryAction -> ByteString

generateActionResponseXml _ st os ContentDirectoryGetSystemUpdateId =
  generateResponseXml body
  where
    body = ( Elem (N "u:GetSystemUpdateIDResponse") [ serviceNs "u" st ]
             [ textElement "Id" $ show $ systemUpdateId os ] )

generateActionResponseXml cfg st os (ContentDirectoryBrowse ba) =
  generateBrowseResponseXml cfg st os ba

generateActionResponseXml _ st _ ContentDirectoryGetSearchCapabilities =
  generateResponseXml body
  where
    body = ( Elem (N "u:GetSearchCapabilitiesResponse") [ serviceNs "u" st ]
             [ emptyElement "SearchCaps" ]   -- No search capabilities (CD/§2.5.18)
           )

generateActionResponseXml _ st _ ContentDirectoryGetSortCapabilities =
  generateResponseXml body
  where
    body = ( Elem (N "u:GetSortCapabilitiesResponse") [ serviceNs "u" st ]
             [ emptyElement "SortCaps" ]   -- No sorting capabilities (CD/§2.5.19)
           )

generateSoapEnvelope :: Element () -> Document ()
generateSoapEnvelope bodyE =
  ( mkDocument
    ( Elem (N "s:Envelope") [ soapNs, soapEncodingStyle ]
      [ simpleElement "s:Body" [elementToContent bodyE] ]
    )
  )
    where
      soapNs = attribute "xmlns:s" $ (urlPrefix ++ "/envelope/")
      soapEncodingStyle = attribute "s:encodingStyle" $ printf "%s/encoding/" urlPrefix
      urlPrefix = "http://schemas.xmlsoap.org/soap"


generateObjectElement :: Configuration -> Objects -> (ObjectId, Object) -> Element ()
generateObjectElement cfg objects (oid, o) =
    ( Elem en (as ++ eas)
      ( [ textElement "dc:title" $ sanitizeXmlChars $ objectTitle od
        , textElement "upnp:class" $ getObjectClassName o
        ] ++ (map elementToContent ee)
      )
    )
    where
      od = getObjectData o
      en = N $ getObjectElementName o
      ee = generateExtraElements cfg (oid,o)
      as = [ attribute "id" $ T.unpack oid
           , attribute "parentID" $ T.unpack $ objectParentId od
           ]
      eas = generateExtraAttributes objects (oid,o)


-- Generate any attributes required for any given object.
-- (Apart from the attributes of the 'object' class itself.)
generateExtraAttributes :: Objects -> (ObjectId, Object) -> [Attribute]
generateExtraAttributes objects (oid, (Container,_)) = genContainerAttributes objects oid
generateExtraAttributes objects (oid, (ContainerStorageFolder,_)) = genContainerAttributes objects oid
generateExtraAttributes _ (_, (ItemMusicTrack,_)) = genItemAttributes
generateExtraAttributes _ (_, (ItemVideoMovie,_)) = genItemAttributes

-- Generate content URL

-- Generate any extra elements for any given object.
generateExtraElements :: Configuration -> (ObjectId, Object) -> [Element ()]
generateExtraElements _ (oid, (Container,_)) = []
generateExtraElements _ (oid, (ContainerStorageFolder,_)) = []
generateExtraElements cfg (oid, (ItemMusicTrack,d)) = generateExtraElementsForFile cfg oid d
generateExtraElements cfg (oid, (ItemVideoMovie,d)) = generateExtraElementsForFile cfg oid d

generateExtraElementsForFile :: Configuration -> ObjectId -> ObjectData -> [Element ()]
generateExtraElementsForFile cfg oid d =
  [ Elem (N "res")
    [ attribute "protocolInfo" protocolInfo
    , attribute "size" $ printf "%d" $ objectFileSize d ] -- TODO: should be disabled by Transcoding flag!
    [ contentUrl ]
  ]
    where
      mimeType = B8.unpack $ objectMimeType d
      protocolInfo = generateProtocolInfo cfg False mimeType Nothing  -- TODO: profileId
      contentUrl = text $ show $ mkURI ["content", T.unpack $ oid] $ httpServerBase cfg

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
              join ";" $ mapMaybe (\(n,v) -> mapMaybe1 (\v' -> n ++ "=" ++ v') v) fields
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
genContainerAttributes :: Objects -> ObjectId -> [Attribute]
genContainerAttributes objects oid =
    [ attribute "searchable" "0"
    , attribute "restricted" "0"
    , attribute "childCount" $ show $ getNumberOfChildren objects oid
    ]

-- Generate extra attributes for items.
genItemAttributes :: [Attribute]
genItemAttributes = []

-- Create the DIDL result object.
mkDidl :: [Element ()] -> Element ()
mkDidl es =
  ( Elem (N "DIDL-Lite")
    [ attribute "xmlns:dc"   "http://purl.org/dc/elements/1.1/"
    , attribute "xmlns:upnp" "urn:schemas-upnp-org:metadata-1-0/upnp/"
    , attribute "xmlns"      "urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/" ]
    ( map elementToContent es )
  )


-- Slice out a portion of a list.
slice :: Int -> Int -> [a] -> [a]
slice i n = take n . drop i
