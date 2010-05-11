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

module Object ( Objects( systemUpdateId )
              , Object
              , ObjectType(..)
              , ObjectData(..)
              , getObjectData
              , getObjectElementName
              , getObjectClassName
              , getChildren
              , getNumberOfChildren
              , scanDirectory
              , findByObjectId
              , findExistingByObjectId
              ) where

import Action
import Data.Map (Map)
import qualified Data.Map as Map
import DirectoryUtils
import System.FilePath
import MimeType
import Data.Int
import System.Posix
import Text.Printf
import StorableExtra
import Text.Regex
import RegexExtra

-- Root object id is defined by CD/§2.7.4.2.
rootObjectId :: ObjectId
rootObjectId = "0"

-- Root object parent id is defined by CD/§2.4.2, table 2.
rootObjectParentId :: ObjectId
rootObjectParentId = "-1"

-- Dispatch table for selecting item type from MIME type.
objectTypeTable :: [(Regex, ObjectType)]
objectTypeTable = [ (mkRegex "^video/.*", ItemVideoMovie)
                  , (mkRegex "^audio/.*", ItemMusicTrack)
                  , (mkRegex "^inode/directory$", ContainerStorageFolder)
                  ]

-- An Objects is an abstract data type containing a set of
-- objects.
data Objects = Objects 
    { mapIdToObject :: Map ObjectId Object
    , mapParentToChildren :: Map ObjectId [ObjectId]
    , systemUpdateId :: Int64
    }
               deriving (Show)

-- Object data which applies for all objects.
data ObjectData = MkObjectData 
    { objectParentId :: ObjectId                -- Object ID of parent object.
    , objectTitle :: String                     -- Title of the object.
    , objectFileName :: FilePath                -- Physical file.
    , objectFileSize :: Integer                 -- The size of the physical file.
    , objectLastModified :: Int64
    , objectMimeType :: String                  -- MIME type of the file.
    }
                  deriving (Show)


data ObjectType = Container
                | ContainerStorageFolder
                | ItemMusicTrack
                | ItemVideoMovie
              deriving (Show)
                
-- We can serve different types of objects.
type Object = (ObjectType, ObjectData)

-- Get the upnp class name of the object.
getObjectClassName :: Object -> String
getObjectClassName (Container,_) = "object.container"
getObjectClassName (ContainerStorageFolder,_) = "object.container.storageFolder"
getObjectClassName (ItemMusicTrack,_) = "object.item.audioItem.musicTrack"
getObjectClassName (ItemVideoMovie,_) = "object.item.videoItem.movie"

-- Get the element name for the object.
getObjectElementName :: Object -> String
getObjectElementName (Container,_) = "container"
getObjectElementName (ContainerStorageFolder,_) = "container"
getObjectElementName (ItemMusicTrack,_) = "item"
getObjectElementName (ItemVideoMovie,_) = "item"

-- Get the object data field of the object.
getObjectData :: Object -> ObjectData
getObjectData = snd

-- Get the children object of a given object.
getChildren :: Objects -> ObjectId -> [(ObjectId,Object)]
getChildren os pid =
    case Map.lookup pid $ mapParentToChildren os of
      Just cs -> map (\oid -> (oid, findExistingByObjectId oid os)) cs
      Nothing -> []

-- Get the number of children of a given object.
getNumberOfChildren :: Objects -> ObjectId -> Int
getNumberOfChildren os =
    length . Object.getChildren os    -- TODO: Keep track of length instead?

-- Find object by object ID.
findByObjectId :: ObjectId -> Objects -> Maybe Object
findByObjectId oid = Map.lookup oid . mapIdToObject

-- Find object which is known to exist by object ID.
findExistingByObjectId :: ObjectId -> Objects -> Object
findExistingByObjectId oid os =
    case findByObjectId oid os of
      Just x -> x
      Nothing -> error $ printf "Couldn't find object '%s'" oid

-- Accumulator function for building the basic list of files/directories.
scanFile :: [(ObjectId, Object)] -> [(ObjectId, Object)] -> FilePath -> IO [(ObjectId, Object)]
scanFile parentObjects objects fp = do
  -- Find parent's object Id.
  let kp = case parentObjects of
             [] -> rootObjectId
             ((oid,_):_) -> oid
  -- Compute object id for the directory entry.
  -- FIXME: Should handle 'file gone missing' -- simply don't prepend an 
  -- object in that case.
  st <- getFileStatus fp 
  deviceId <- toHexString $ deviceID st
  fileId <- toHexString $ fileID st
  let oid = printf "%s,%s" deviceId fileId
  -- Compute the update ID.
  let lastModified = round' $ toRational $ modificationTime st
  -- Compute file size.
  let sz = (fromIntegral . System.Posix.fileSize) st
  -- Compute misc. attributes.
  let _title = dropExtension $ takeFileName fp
  -- Start by guessing mime type.
  let mimeType = if isDirectory st then
                     "inode/directory"       -- Directories are special.
                 else guessMimeType fp
  -- Construct object data.
  let objectData = MkObjectData kp _title fp sz lastModified mimeType
  -- Add the directory entry to the current accumulator.
  return $ case dispatch objectTypeTable mimeType of
             Just (objectType,_) -> (oid, (objectType,objectData)) : objects
             Nothing -> objects
  where
    round' :: Rational -> Int64          -- Dummy to avoid warning
    round' = round

  
-- Function for building the Object tree structure.
scanDirectory :: FilePath -> IO Objects
scanDirectory d = do
  objects <- walkTree [] scanFile d
  -- Add the special top-level root item.
  let o' = (rootObject : objects)
  -- Construct a map from parents to children.
  let mapParentToChildrenX = foldl p2c Map.empty o'

  let getModificationTime = objectLastModified . getObjectData . snd

  return Objects 
             { mapIdToObject = Map.fromList o'
             , mapParentToChildren = mapParentToChildrenX
             , systemUpdateId = maximum $ map getModificationTime o'
             }
  where 
    -- The root object is fixed.
    rootObject = 
        (rootObjectId,    
           (Container, MkObjectData { objectParentId = rootObjectParentId
                                    , objectTitle = "root"
                                    , objectFileName = "root"
                                    , objectFileSize = 0
                                    , objectLastModified = 0
                                    , objectMimeType = "inode/directory" }))

    p2c acc (oid, o) =
        Map.alter (\x -> case x of
                           Nothing -> Just [oid]
                           Just cs -> Just (oid:cs)) pid acc
        where
          pid = objectParentId $ getObjectData o
