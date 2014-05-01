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

module HttpExtra ( parseRangeHeader
                 ) where

import           Data.ByteString.Char8 (isPrefixOf, ByteString)
import qualified Data.ByteString.Char8 as B8

-- Strip a prefix of a byte string and return the suffix (if the
-- prefix was actually a prefix).
parseLiteral :: ByteString -> ByteString -> Maybe ByteString
parseLiteral p s =
    case p `isPrefixOf` s of
      False -> Nothing
      True -> Just $ B8.drop (B8.length p) s

-- Parse the Range header sent by the PS/3. The Range header that the
-- PS/3 send is always in a very simple format, so we shortcut this to
-- avoid the complexity of full HTTP/1.1 range headers.
parseRangeHeader :: ByteString -> Maybe (Integer, Maybe Integer)
parseRangeHeader s = do
  -- Extract the first part of the range
  s' <- parseLiteral "bytes=" s
  (startI, s'') <- B8.readInteger s'
  s''' <- parseLiteral "-" s''
  -- The second part of the range is optional.
  if B8.length s''' == 0 then
      return $ (startI, Nothing)
  else do
    (endI, _) <- B8.readInteger s'''
    return (startI, Just endI)
