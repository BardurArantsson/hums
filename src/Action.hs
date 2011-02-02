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

module Action ( ContentDirectoryAction(..)
              , ConnectionManagerAction(..)
              , Action(..)
              , BrowseAction(..)
              , BrowseParameters(..)
              , ObjectId
              ) where

import Data.ByteString (ByteString)

type ObjectId = ByteString

data BrowseParameters = BrowseParameters
    { objectId :: ObjectId
    , browseFilter :: String
    , startingIndex :: Int
    , requestedCount :: Int
    , sortCriteria :: String
    }
                      deriving (Show)

data BrowseAction = BrowseMetadata BrowseParameters
                  | BrowseDirectChildren BrowseParameters
                  deriving (Show)

-- We currently only support the required (CD1.0) actions.
data ContentDirectoryAction = ContentDirectoryBrowse BrowseAction
                            | ContentDirectoryGetSearchCapabilities
                            | ContentDirectoryGetSortCapabilities
                            | ContentDirectoryGetSystemUpdateId
                              deriving (Show)

data ConnectionManagerAction = ConnectionManagerGetProtocolInfo
                             | ConnectionManagerPrepareForConnection
                             | ConnectionManagerConnectionComplete
                             | ConnectionManagerGetCurrentConnectionIDs
                             | ConnectionManagerGetCurrentConnectionInfo
                             deriving (Show)

data Action = ContentDirectoryAction_ ContentDirectoryAction
            | ConnectionManagerAction_ ConnectionManagerAction
              deriving (Show)
