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

module DirectoryUtils ( walkTree
                      ) where

import System.Directory
import Control.Monad
import System.FilePath
import Data.List

-- Performs a pre-order traversal of a directory.
-- It calls f a0 a fp for each file/directory, where
-- a0 is the accumulator as it appeared at the start
-- of iteration of the parent directory, a is the
-- current value of the accumulator and fp is the
-- file name of the file/directory being visited.
walkTree :: a -> (a -> a -> FilePath -> IO a) -> FilePath -> IO a
walkTree s0 f d = do
  -- FIXME: Need to detect loops!
  -- Get files and directories in directory. If that fails
  -- we just pretend there are none.
  allNames <- catch
              (getDirectoryContents d)
              (\e -> do
                  putStrLn $ "Error retrieving directory contents: " ++ show e -- Log errors
                  return [])
  -- Filter out the special directories.
  let names = sort $ filter (not . isSpecialDirectory) allNames
  -- Produce full names.
  let fullNames = map (combine d) names
  -- Traverse subdirectories and return accumulator.
  foldM traverse s0 fullNames
  where
    traverse s n = do
      isFile <- doesFileExist n
      case isFile of
        True -> f s0 s n
        False -> do
          isDirectory <- doesDirectoryExist n
          case isDirectory of
            True -> do
              s' <- f s0 s n
              walkTree s' f n
            False -> do
              -- Not a directory nor an existing file. Conclusion: A dead symlink.
              putStrLn $ "Ignoring dead symbolic link: " ++ (show n)
              return s

    isSpecialDirectory ".." = True
    isSpecialDirectory "." = True
    isSpecialDirectory _   = False
