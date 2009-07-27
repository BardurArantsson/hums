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

import Text.ParserCombinators.Parsec

parseRange :: GenParser Char a (Maybe Integer, Maybe Integer)
parseRange = do
    choice [parseFullRange,
            parseEndRange, 
            parseSingleByteRange]
 
parseFullRange :: GenParser Char a (Maybe Integer, Maybe Integer)
parseFullRange = do
  i1 <- parseInteger
  char '-'
  i2 <- optionMaybe parseInteger
  return (Just i1, i2)
  
parseEndRange :: GenParser Char a (Maybe Integer, Maybe Integer)
parseEndRange = do
  char '-'
  i1 <- parseInteger
  return (Just $ -i1, Nothing)

parseSingleByteRange :: GenParser Char a (Maybe Integer, Maybe Integer)
parseSingleByteRange = do
  i1 <- parseInteger
  char '-'
  return (Just $ i1, Just $ i1)

parseInteger :: GenParser Char a Integer
parseInteger = do
  ds <- many1 digit
  return $ ((read ds) :: Integer)

parseRanges :: GenParser Char a [(Maybe Integer, Maybe Integer)]
parseRanges = do
  string "bytes="
  rs <- parseRange `sepBy` (char ',')
  return rs

parseRangeHeader :: String -> [(Maybe Integer, Maybe Integer)]
parseRangeHeader s =
    case parse parseRanges "-" s of
      Left err -> []     -- Ignore if we can't parse
      Right rs -> rs
