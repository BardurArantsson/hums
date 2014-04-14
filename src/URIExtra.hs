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

module URIExtra ( mkURIReference
                , mkURI
                ) where

import Network.URI (URI, relativeTo, parseRelativeReference, escapeURIString, isUnescapedInURI)
import Data.List (foldl', isSuffixOf)

-- Create an URI reference (absolute or relative URI with optional fragment identifier)
-- from an arbitrary string. The string is URI-encoded if necessary.
-- TODO: Document partiality if input string is not valid (as per parseURIReference).
mkURIReference :: String -> URI
mkURIReference s =
    case {- parseURIReference -} parseRelativeReference $ escapeURIString isUnescapedInURI s of
      Just u -> u
      Nothing -> error $ "Invalid URI component '" ++ s ++ "'"

-- Make sure we have a trailing slash on all path components,
-- *except* the last.
addSlashes :: [String] -> [String]
addSlashes [] = []
addSlashes ("":xs) = addSlashes xs                 -- Remove empty components
addSlashes [x] = [x]                              -- Last component
addSlashes (x:y:xs) = addSlash x : addSlashes (y:xs)  -- Other components

-- Add trailing slash to a string unless already present.
addSlash :: String -> String
addSlash s | "/" `isSuffixOf` s = s
addSlash s = s ++ "/"

-- Build a complete URI from a set of path components and a base URI.
mkURI :: [String] -> URI -> URI
mkURI ss u =
    mkURI' u $ map mkURIReference $ addSlashes ss

mkURI' :: URI -> [URI] -> URI
mkURI' =
    foldl' (\a r -> r `relativeTo` a)
