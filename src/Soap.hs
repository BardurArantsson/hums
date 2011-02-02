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

module Soap ( parseControlSoapXml
            ) where

import qualified Data.ByteString.Char8 as B8
import Text.XML.HXT.Core
import Action

-- Utility functions for parsing XML.
atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasLocalPart tag)

text :: ArrowXml a => a XmlTree String
text = (getChildren >>> getText)       -- Gather text from all child nodes.
       `orElse` (txt "" >>> getText)   -- No child nodes means empty string.

textAtTag :: ArrowXml a => String -> a XmlTree String
textAtTag tag = atTag tag >>> text

numberAtTag :: (ArrowXml a, Num b, Read b) => String -> a XmlTree b
numberAtTag tag =
    atTag tag >>> text >>> arr read

-- Parse the "Browse Flag". CD/§2.5.6
parseBrowseFlag :: ArrowXml a => a XmlTree (BrowseParameters -> BrowseAction)
parseBrowseFlag =
    atTag "BrowseFlag" >>> getChildren >>> (f1 `orElse` f2 `orElse` f3)
    where
      f1 = (hasText ((==) "BrowseMetadata")) >>> constA BrowseMetadata
      f2 = (hasText ((==) "BrowseDirectChildren")) >>> constA BrowseDirectChildren
      f3 = error "Protocol error: invalid browse flag"

{-

   Content Directory SOAP messages.

-}
parseCDBrowse :: ArrowXml a => a XmlTree ContentDirectoryAction
parseCDBrowse =
    atTag "Browse" >>>
          proc l -> do
            oid        <- textAtTag "ObjectID"
                 `orElse` textAtTag "ContainerID" {- XBox-360 -}  -< l
            bf         <- parseBrowseFlag                           -< l
            flt        <- textAtTag "Filter"                        -< l
            si         <- numberAtTag "StartingIndex"               -< l
            rq         <- numberAtTag "RequestedCount"              -< l
            sc         <- textAtTag "SortCriteria"                  -< l
            returnA    -< let bps = BrowseParameters (B8.pack oid) flt si rq sc in
                            ContentDirectoryBrowse $ bf bps

parseCDSearchCapabilities :: ArrowXml a => a XmlTree ContentDirectoryAction
parseCDSearchCapabilities =
    atTag "GetSearchCapabilities" >>>
          proc _ -> returnA -< ContentDirectoryGetSearchCapabilities

parseCDGetSortCapabilities :: ArrowXml a => a XmlTree ContentDirectoryAction
parseCDGetSortCapabilities =
    atTag "GetSortCapabilities" >>>
          proc _ -> returnA -< ContentDirectoryGetSortCapabilities

parseCDGetSystemUpdateId :: ArrowXml a => a XmlTree ContentDirectoryAction
parseCDGetSystemUpdateId =
    atTag "GetSystemUpdateID" >>>
          proc _ -> returnA -< ContentDirectoryGetSystemUpdateId

parseContentDirectoryQueryXml :: ArrowXml a => a XmlTree Action
parseContentDirectoryQueryXml =
    proc l -> do
      x <- helper    -< l
      returnA -< ContentDirectoryAction_ x
    where helper =
               parseCDBrowse `orElse` parseCDSearchCapabilities
                             `orElse` parseCDGetSortCapabilities
                             `orElse` parseCDGetSystemUpdateId

{-

  Connection Manager SOAP messages.

-}
parseCMGetProtocolInfo :: ArrowXml a => a XmlTree ConnectionManagerAction
parseCMGetProtocolInfo =
    atTag "GetProtocolInfo" >>>
          proc _ -> returnA -< ConnectionManagerGetProtocolInfo

parseCMPrepareForConnection :: ArrowXml a => a XmlTree ConnectionManagerAction
parseCMPrepareForConnection =
    atTag "PrepareForConnection" >>>
          proc _ -> returnA -< ConnectionManagerPrepareForConnection

parseCMConnectionComplete :: ArrowXml a => a XmlTree ConnectionManagerAction
parseCMConnectionComplete =
    atTag "ConnectionComplete" >>>
          proc _ -> returnA -< ConnectionManagerConnectionComplete

parseCMGetCurrentConnectionIDs :: ArrowXml a => a XmlTree ConnectionManagerAction
parseCMGetCurrentConnectionIDs =
    atTag "GetCurrentConnectionIDs" >>>
          proc _ -> returnA -< ConnectionManagerGetCurrentConnectionIDs

parseCMGetCurrentConnectionInfo :: ArrowXml a => a XmlTree ConnectionManagerAction
parseCMGetCurrentConnectionInfo =
    atTag "GetCurrentConnectionInfo" >>>
          proc _ -> returnA -< ConnectionManagerGetCurrentConnectionInfo

parseConnectionManagerQueryXml :: ArrowXml a => a XmlTree Action
parseConnectionManagerQueryXml =
    proc l -> do
      x <- helper     -< l
      returnA -< ConnectionManagerAction_ x
    where helper =
              parseCMGetProtocolInfo `orElse` parseCMPrepareForConnection
                                     `orElse` parseCMConnectionComplete
                                     `orElse` parseCMGetCurrentConnectionIDs
                                     `orElse` parseCMGetCurrentConnectionInfo

parseQueryXml :: ArrowXml a => a XmlTree Action
parseQueryXml =
    parseContentDirectoryQueryXml `orElse` parseConnectionManagerQueryXml

{-

  Parse SOAP messages.

-}

parseControlSoapXml :: String -> IO (Maybe Action)
parseControlSoapXml xml = do
    as <- runX $ readString conf xml >>> parseQueryXml
    case as of
      (a:_) -> return $ Just a
      _     -> return $ Nothing
    where
      conf :: SysConfigList
      conf = [ withValidate no
             , withCheckNamespaces False
             ]
