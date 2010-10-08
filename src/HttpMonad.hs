module HttpMonad ( HttpT
                 , runHttp
                 , HttpResponseCode(..)
                 , addHeader
                 , setResponseCode
                 , setContentLength
                 , writeToBody
                 , writeFileToBody
                 , logMessage
                 , getRequest
                 , ifRegex
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
import Data.ByteString.UTF8 (fromString)
import SendFile (sendFile)
import Text.Printf (printf)
import Text.Regex (matchRegex, mkRegex)

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
    lift $ liftIO $ output $ fromString sResponse
    -- We've flushed the headers.
    modify (\s -> s { hHeadersFlushed = True })

-- Set the response code.
setResponseCode :: Monad m => HttpResponseCode -> HttpT m ()
setResponseCode c = HttpT $ lift $ do
  modify $ \st -> st { hStatusCode = c }

-- Write to body.
writeToBody :: (Functor m, MonadIO m, Monad m) => ByteString -> HttpT m ()
writeToBody buf = HttpT $ do
  output <- lift $ hOutput <$> get
  unHttp $ flushHeaders
  lift $ liftIO $ output buf

-- Write the contents of a file to body.
writeFileToBody :: (Functor m, MonadIO m, Monad m) => FilePath -> Integer -> Integer -> HttpT m ()
writeFileToBody buf offset count = HttpT $ do
  output <- lift $ hOutput <$> get
  unHttp $ flushHeaders
  lift $ liftIO $ sendFile output buf offset count

-- Logging.
logMessage :: (Monad m, MonadIO m) => String -> HttpT m ()
logMessage s = HttpT $ liftIO $ putStrLn s

-- Get request.
getRequest :: (Functor m, Monad m) => HttpT m (Request String)
getRequest = HttpT $ ask

-- Match request prefix.
ifRegex :: (Functor m, Monad m) => String -> ([String] -> HttpT m ()) -> HttpT m () -> HttpT m ()
ifRegex r ifMatch ifNoMatch = do
  request <- getRequest
  case matchRegex re $ urlDecode $ uriPath $ rqURI request of
    Just gs -> ifMatch gs
    Nothing -> ifNoMatch
  where
    re = mkRegex r
