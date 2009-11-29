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

module StorableExtra ( toHexString ) where

import Storable
import Data.Word
import Foreign.Marshal.Utils (with)
import Text.Printf
import Control.Monad

-- Convert a storable to an [Word8].
toWord8Array :: Storable a => a -> IO [Word8]
toWord8Array a =
  loop 0
  where
    loop i =
          if i < sizeOf a then do
                            x <- with a (\aptr -> peekByteOff aptr i) :: IO Word8
                            xs <- loop $ i+1
                            return (x : xs)
          else
              return []
  

-- Convert a storable to a string of hex digits.
toHexString :: Storable a => a -> IO String
toHexString a = do
  bytes <- toWord8Array a
  let s = concatMap (printf "%02x") bytes :: String 
  return s
