{-
    hums - The Haskell UPnP Server
    Copyright (C) 2009, 2012 Bardur Arantsson <bardur@scientician.net>

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

import           Control.Exception (catch, SomeException)
import           Control.Monad
import           Data.List
import qualified Data.Text as T
import           Filesystem (listDirectory)
import           Filesystem.Path (FilePath, filename)
import           Filesystem.Path.CurrentOS (toText, encode)
import           Prelude hiding (FilePath)
import           System.Posix.ByteString (FileStatus)
import qualified System.Posix.ByteString as P

-- Sorting function.
compareNames :: FilePath -> FilePath -> Ordering
compareNames a b = compare (f a) (f b) where
    f = T.toCaseFold . either id id . toText . filename

-- Performs a pre-order traversal of a directory.
-- It calls f a0 a fp for each file/directory, where
-- a0 is the accumulator as it appeared at the start
-- of iteration of the parent directory, a is the
-- current value of the accumulator and fp is the
-- file name of the file/directory being visited.
walkTree :: a -> (a -> a -> FileStatus -> FilePath -> IO a) -> FilePath -> IO a
walkTree s0 f d = do
  -- FIXME: Need to detect loops!
  -- Get files and directories in directory. If that fails
  -- we just pretend there are none.
  allNames <- catch
              (listDirectory d)
              (\(e :: SomeException) -> do
                  putStrLn $ "Error retrieving directory contents: " ++ show e -- Log errors
                  return [])
  -- Sort
  let sortedNames = sortBy compareNames allNames
  -- Traverse subdirectories and return accumulator.
  foldM traverse s0 sortedNames
  where
    traverse s n = do
      st <- P.getFileStatus $ encode n
      if P.isRegularFile st
        then f s0 s st n
        else if P.isDirectory st
          then do
             s' <- f s0 s st n
             walkTree s' f n
          else do
              -- Not a directory nor an existing file. Conclusion: A dead symlink.
              putStrLn $ "Ignoring dead symbolic link: " ++ (show n)
              return s
