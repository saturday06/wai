{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Blaze.ByteString.Builder (fromStorables)
import Blaze.ByteString.Builder.HTTP
import Control.Exception
import Criterion.Main
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Data.ByteString.Builder
import Data.ByteString.Internal (ByteString(..))
import Data.Traversable
import Data.Word8
import Data.Maybe
import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.IO
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Socket
import Network.HTTP.Types
import Network.Wai.Handler.Warp.Types
import System.IO
import Foreign.Ptr (plusPtr)
import System.IO.Error
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket.ByteString as BS
import qualified Data.ByteString.Char8 as CBS

-- $setup
-- >>> :set -XOverloadedStrings

readBufferSize :: Int
readBufferSize = 100 * 1024 * 1024

writeBufferSize :: Int
writeBufferSize = 100 * 1024 * 1024

localhost :: HostAddress
localhost = tupleToHostAddress (127, 0, 0, 1)

streamApp :: Application
streamApp request respond = respond $ responseStream status200 [] $ \write flush -> do
  let
    loop 0 = return ()
    loop c = do
      write responseBody
      loop $ c - 1
  loop responseCount
  where
    responseBody = fromStorables $ replicate responseSize 'x'
    responseSize = fst $ fromJust $ CBS.readInt $ fromJust $ fromJust $ lookup "size" $ queryString request
    responseCount = fst $ fromJust $ CBS.readInt $ fromJust $ fromJust $ lookup "count" $ queryString request

builderApp :: Application
builderApp request respond = respond $ responseBuilder status200 [] $ byteString BS.empty

lbsApp :: Application
lbsApp request respond = respond $ responseLBS status200 [] LBS.empty

app :: Application
app request = case pathInfo request of
  ["stream"]  -> streamApp request
  ["builder"] -> builderApp request
  ["lbs"]     -> lbsApp request
  _           -> error "Not Found"

benchWarpResponse :: PortNumber -> Buffer -> Socket -> String -> Benchmark
benchWarpResponse port !readBuffer keepaliveSocket path = bgroup ("GET /" ++ path) [
    bench "HTTP/1.0" $ whnfIO $ do
      bracket (socket AF_INET Stream defaultProtocol) close $ \s -> do
        connect s $ SockAddrInet port localhost
        BS.sendAll s $ CBS.pack $ "GET /" ++ path ++ " HTTP/1.0\r\n\r\n"
        recvAll s
  , bench "HTTP/1.1" $ whnfIO $ do
      bracket (socket AF_INET Stream defaultProtocol) close $ \s -> do
        connect s $ SockAddrInet port localhost
        BS.sendAll s $ CBS.pack $ "GET /" ++ path ++ " HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
        recvAll s
{-
  , bench "HTTP/1.1 keep-alive" $ whnfIO $ do
      BS.sendAll keepaliveSocket $ CBS.pack $ "GET /" ++ path ++ " HTTP/1.1\r\nHost: localhost\r\n\r\n"
      recvAllChunk keepaliveSocket 0
-}
  ]
  where
    recvAll s = do
      received <- tryIOError $ recvBuf s readBuffer readBufferSize
      case received of
        Right len -> recvAll s
        Left e -> if isEOFError e then return () else ioError e
    recvAllChunk s off = do
      len <- recvBuf s (readBuffer `plusPtr` off) $ readBufferSize - off
      if off + len > 5
        then do
          let trailer = [_0, _cr, _lf, _cr, _lf]
          tail <- peekArray (length trailer) $ readBuffer `plusPtr` (off + len - (length trailer))
          if trailer == tail then return () else recvAllChunk s $ off + len
        else recvAllChunk s $ off + len

main :: IO ()
main = withSocketsDo $ do
  bracket (mallocBytes readBufferSize) free $ \readBuffer -> do
    let writeBuffer = BS.replicate writeBufferSize _x :: BS.ByteString
    withApplication (pure app) $ \port -> do
      bracket (socket AF_INET Stream defaultProtocol) close $ \keepaliveSocket -> do
        connect keepaliveSocket $ SockAddrInet (fromIntegral port) localhost
        let portNumber = fromIntegral port :: PortNumber
        defaultMain [
            benchWarpResponse portNumber readBuffer keepaliveSocket "stream?size=1024&count=1"
          , benchWarpResponse portNumber readBuffer keepaliveSocket "stream?size=1024&count=100"
          , benchWarpResponse portNumber readBuffer keepaliveSocket "stream?size=8192&count=1"
          , benchWarpResponse portNumber readBuffer keepaliveSocket "stream?size=8192&count=100"
          , benchWarpResponse portNumber readBuffer keepaliveSocket "stream?size=500000&count=1"
          , benchWarpResponse portNumber readBuffer keepaliveSocket "stream?size=500000&count=30"
          ]
