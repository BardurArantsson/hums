{-
    hums - The Haskell UPnP Server
    Copyright (C) 2009-2010 Bardur Arantsson <bardur@scientician.net>

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
module HttpMonad ( HttpT
                 , runHttp
                 , HttpResponseCode(..)
                 , addHeader
                 , setResponseCode
                 , setContentLength
                 , writeToBody
                 , writeFileToBody
                 , logDataLBS
                 , logMessage
                 , getRequest
                 , ifPrefix
                 , ifPath
                 ) where

import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Control.Applicative
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (unless)
import Network.HTTP.Base (ResponseCode, Response(..), Request(..), urlDecode)
import Network.HTTP.Headers (Header(..), HeaderName(..))
import Network.StreamSocket()
import Network.URI (uriPath)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.List (stripPrefix)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import SendFile (sendFile)
import Text.Printf (printf)

-- HTTP response codes.
data HttpResponseCode = OK
                      | PartialContent
                      | NotFound
                      | InternalServerError
                      | NotImplemented

-- Convert HTTP response code to a numeric code and string.
mapStatusCode :: HttpResponseCode -> (ResponseCode, String)
mapStatusCode OK                  = ((2,0,0), "OK")
mapStatusCode NotFound            = ((4,0,4), "Not Found")
mapStatusCode InternalServerError = ((5,0,4), "Internal Server Error")
mapStatusCode NotImplemented      = ((5,0,1), "Not Implemented")
mapStatusCode PartialContent      = ((2,0,6), "Partial Content")

-- State of the HTTP writer monad.
data HttpState =
  HttpState { hHeaders :: [Header]
            , hHeadersFlushed :: Bool
            , hStatusCode :: HttpResponseCode
            , hOutput :: ByteString -> IO ()
            }

type Req = Request String

-- Newtype wrapper for HttpT for implementation hiding.
newtype HttpT m a = HttpT { unHttp :: ReaderT Req (StateT HttpState m) a }
                        deriving (Functor, Monad)

-- Instances
instance MonadTrans HttpT where
   lift m = HttpT $ (lift . lift) m

-- Run a Http computation.
runHttp :: (Functor m, Monad m, MonadIO m) => (ByteString -> IO ()) -> Request String -> HttpT m a -> m a
runHttp output request m = do
  evalStateT (runReaderT (unHttp m') request) s0
  where
    s0 = HttpState [] False OK output
    m' = do
      a <- m
      flushHeaders -- Just in case we haven't written any data.
      return a

-- Add header to the response.
addHeader :: Monad m => Header -> HttpT m ()
addHeader header = HttpT $ do
  lift $ modify $ \st -> st { hHeaders = header : hHeaders st }

-- Set content length.
setContentLength :: Monad m => Maybe Integer -> HttpT m ()
setContentLength (Just n) = addHeader (Header HdrContentLength $ printf "%d" n) -- FIXME: Replace!
setContentLength Nothing  = return () -- FIXME: Remove!


-- Flush all the headers.
flushHeaders :: (Functor m, Monad m, MonadIO m) => HttpT m ()
flushHeaders = HttpT $ lift $ do
  -- Ignore if already flushed.
  alreadyFlushed <- hHeadersFlushed <$> get
  unless alreadyFlushed $ do
    -- Generate the headers.
    output <- hOutput <$> get
    headers <- hHeaders <$> get
    (code,reason) <- fmap mapStatusCode $ hStatusCode <$> get
    let sResponse = show $ Response { rspCode = code
                                    , rspReason = reason
                                    , rspHeaders = headers
                                    , rspBody = "" }
    -- Output the headers.
    lift $ liftIO $ output $ encodeUtf8 $ T.pack sResponse
    -- We've flushed the headers.
    modify (\s -> s { hHeadersFlushed = True })

-- Set the response code.
setResponseCode :: Monad m => HttpResponseCode -> HttpT m ()
setResponseCode c = HttpT $ lift $ do
  modify $ \st -> st { hStatusCode = c }

-- Write to body.
writeToBody :: (Functor m, MonadIO m, Monad m) => L.ByteString -> HttpT m ()
writeToBody buf = HttpT $ do
  output <- lift $ hOutput <$> get
  unHttp $ flushHeaders
  lift $ liftIO $ output $ S.concat $ L.toChunks buf     -- FIXME: Maybe change "output" to use lazy?

-- Write the contents of a file to body.
writeFileToBody :: (Functor m, MonadIO m, Monad m) => FilePath -> Integer -> Integer -> HttpT m ()
writeFileToBody buf offset count = HttpT $ do
  output <- lift $ hOutput <$> get
  unHttp $ flushHeaders
  lift $ liftIO $ sendFile output buf offset count

-- Logging.
logMessage :: (Monad m, MonadIO m) => String -> HttpT m ()
logMessage s = HttpT $ liftIO $ putStrLn s

logDataLBS :: (Monad m, MonadIO m) => L.ByteString -> HttpT m ()
logDataLBS s = HttpT $ liftIO $ L.putStrLn s

-- Get request.
getRequest :: (Functor m, Monad m) => HttpT m (Request String)
getRequest = HttpT $ ask

-- Dispatch based on a test-and-forward function.
dispatchOn :: (Functor m, Monad m) => (String -> Maybe a) -> (a -> HttpT m ()) -> HttpT m () -> HttpT m ()
dispatchOn p ifMatch ifNoMatch = do
  request <- getRequest
  case p $ urlDecode $ uriPath $ rqURI request of
    Just r -> ifMatch r
    Nothing -> ifNoMatch

-- Match request prefix. The request path is stripped of the prefix
-- before being forwarded to the handler.
ifPrefix :: (Functor m, Monad m) => String -> (String -> HttpT m ()) -> HttpT m () -> HttpT m ()
ifPrefix p = dispatchOn (stripPrefix p)

-- Match full request path.
ifPath :: (Functor m, Monad m) => String -> HttpT m () -> HttpT m () -> HttpT m ()
ifPath p ifMatch ifNoMatch =
  dispatchOn predicate ifMatch' ifNoMatch
  where
    predicate p' = if p == p' then Just () else Nothing
    ifMatch' _ = ifMatch
