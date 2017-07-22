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

app :: Application
app req res = res $ responseBuilder status200 [] responseBody
  where
    responseBody = fromStorables $ replicate responseSize 'x'
    responseSize = fst $ fromJust $ CBS.readInt $ fromJust $ fromJust $ lookup "size" $ queryString req

app2 :: BS.ByteString -> Application
app2 !buf req res = res $ responseStream status200 [] $ \write flush -> do
    write $ byteString $ PS ptr off responseSize
  where
    responseSize = fst $ fromJust $ CBS.readInt $ fromJust $ fromJust $ lookup "size" $ queryString req
    PS ptr off len = buf

benchWarpResponse :: PortNumber -> Buffer -> Socket -> Int -> Benchmark
benchWarpResponse port !readBuffer keepaliveSocket size = bgroup (show size ++ " bytes response") [
    bench "HTTP/1.0" $ whnfIO $ do
      bracket (socket AF_INET Stream defaultProtocol) close $ \s -> do
        connect s $ SockAddrInet port localhost
        BS.sendAll s $ CBS.pack $ "GET /?size=" ++ show size ++ " HTTP/1.0\r\n\r\n"
        recvAll s
  , bench "HTTP/1.1" $ whnfIO $ do
      bracket (socket AF_INET Stream defaultProtocol) close $ \s -> do
        connect s $ SockAddrInet port localhost
        BS.sendAll s $ CBS.pack $ "GET /?size=" ++ show size ++ " HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
        recvAll s
{-
  , bench "HTTP/1.1 keep-alive" $ whnfIO $ do
      BS.sendAll keepaliveSocket $ CBS.pack $ "GET /?size=" ++ show size ++ " HTTP/1.1\r\nHost: localhost\r\n\r\n"
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
            benchWarpResponse portNumber readBuffer keepaliveSocket 1024
          , benchWarpResponse portNumber readBuffer keepaliveSocket $ 1024 * 1024
          , benchWarpResponse portNumber readBuffer keepaliveSocket $ 50 * 1024 * 1024
          ]
