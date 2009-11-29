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

module RegexExtra ( dispatch )
    where

import Text.Regex

dispatch :: [(Regex, a)] -> String -> Maybe (a, [String])
dispatch t s = 
    case findFirst f t of
      Nothing -> Nothing
      Just (v,gs) -> Just (v, gs)
    where
      f (r,v) = case matchRegex r s of
                  Just gs -> Just (v,gs)
                  Nothing -> Nothing
                
findFirst :: (a -> Maybe b) -> [a] -> Maybe b
findFirst _ [] = Nothing
findFirst f (x:xs) =
    case f x of
      Just v -> Just v
      Nothing -> findFirst f xs
