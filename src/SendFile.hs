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

--
-- This exists because Network.Socket.SendFile isn't usable on Linux as of 0.4.
--
module SendFile ( sendFile' 
                ) where

import Data.ByteString as BS (hGet, length)
import Network.Socket.ByteString (sendAll)
import Network.Socket (Socket)
import System.IO (hSeek, withFile, IOMode(..), SeekMode(..))

sendFile' :: Socket -> FilePath -> Integer -> Integer -> IO ()
sendFile' outs inp off count = do
  withFile inp ReadMode (\h -> do
    hSeek h AbsoluteSeek off
    rsend h count)
    where rsend h 0        = return ()
          rsend h reqBytes = do
              let bytes = min 32768 reqBytes :: Integer
              buf <- hGet h (fromIntegral bytes)
              sendAll outs buf
              rsend h (reqBytes - (fromIntegral $ BS.length buf))
