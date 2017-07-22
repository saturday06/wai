{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import           Blaze.ByteString.Builder (Builder, fromByteString, fromLazyByteString, fromStorables)
import           Control.Monad
import           Control.Exception
import           Data.ByteString.Internal (ByteString(..))
import           Data.Maybe
import           Data.Streaming.Network
import           Data.String
import           Data.Word8
import           Network.HTTP.Client
import           Network.HTTP.Client.Internal hiding (Connection)
import           Network.HTTP.ReverseProxy
import           Network.HTTP.Types
import           Network.Wai as Wai
import           Network.Wai.Handler.Warp as Warp
import           Network.Wai.Handler.Warp.Types
import           Network.Wai.Handler.Warp.Run
import           Network.Wai.Handler.Warp.Buffer hiding (bufferSize)
import           Network.Wai.Handler.Warp.SendFile
import           Network.Wai.Handler.Warp.Recv
import           Network.Wai.Handler.Warp.Internal (runSettingsConnection)
import           Network.Socket
import qualified Network.Socket.ByteString as Sock
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Lazy as LBS

goBufferSize :: Int
goBufferSize = 32 * 1024

blazeContent :: Builder
!blazeContent = mconcat $ replicate (2 * 1024 * 1024) "x"

main5 :: IO ()
main5 = do
  let settings = Warp.setPort 3335 defaultSettings
  runSettings settings $ blazeApp blazeContent

blazeApp :: Builder -> Application
blazeApp builder req res = res $ responseBuilder status200 [] builder

main6 :: IO ()
main6 = do
  let settings = Warp.setPort 3335 defaultSettings
  runSettings settings $ blazeStreamApp blazeContent

blazeStreamApp :: Builder -> Application
blazeStreamApp builder req res = res $ responseStream status200 [] $ \write flush -> do
    write builder

writeBufferSize :: Int
writeBufferSize = 50 * 1024 * 1024

main4 :: IO ()
main4 = do
  let settings = Warp.setPort 3335 defaultSettings
  let writeBuffer = BS.replicate writeBufferSize _x :: BS.ByteString
  runSettings settings $ app writeBuffer

app :: BS.ByteString -> Application
app !buf req res = res $ responseStream status200 [] $ \write _ -> do
    write $ fromByteString $ PS ptr off responseSize
  where
    responseSize = fst $ fromJust $ CBS.readInt $ fromJust $ fromJust $ lookup "size" $ Wai.queryString req
    PS ptr off len = buf

mainx :: IO ()
mainx = do
  manager <- newManager $ defaultManagerSettings {
      managerConnCount = 5000
    , managerIdleConnectionCount = 5000
    , managerRawConnection = return $ openSocketConnectionSize (const $ return ()) goBufferSize
    }
  let settings = Warp.setPort 3335 defaultSettings
  bracket (bindPortTCP (Warp.getPort settings) (Warp.getHost settings)) close $ \serverSocket -> do
    -- setInstallShutdownHandler settings close serverSocket
    let getConnection = do
            (clientSocket, clientSocketAddress) <- accept serverSocket
            setSocketCloseOnExec clientSocket
            setSocketOption clientSocket NoDelay 1
            connection <- do
              let
                bufferSize = goBufferSize + 4096
                sendAll = Sock.sendAll clientSocket
              bufferPool <- newBufferPool
              writeBuf <- allocateBuffer bufferSize
              return Connection {
                  connSendMany = Sock.sendMany clientSocket
                , connSendAll = sendAll
                , connSendFile = sendFile clientSocket writeBuf bufferSize sendAll
                , connRecv = receive clientSocket bufferPool
                , connClose = close clientSocket
                , connFree = freeBuffer writeBuf
                , connRecvBuf = receiveBuf clientSocket
                , connWriteBuffer = writeBuf
                , connBufferSize = bufferSize
                }
            return (connection, clientSocketAddress)
    runSettingsConnection settings getConnection $ proxyApp manager

proxyApp :: Manager -> Application
proxyApp manager request sendResponse = do
  sendResponse $ responseStream status200 [] $ \write _ -> do
    upstreamRequest <- (\r -> r { path = rawPathInfo request }) <$> parseRequest "http://127.0.0.1:8080"
    withResponse upstreamRequest manager $ \upstreamResponse -> do
      let loop = do
            body <- brRead (responseBody upstreamResponse)
            if BS.null body then return () else do
              write $ fromByteString body
              loop
      loop

main :: IO ()
main = do
  let settings = Warp.setPort 3335 defaultSettings
  let dest = WPRProxyDest ProxyDest { pdHost = "127.0.0.1", pdPort = 8080 }
  manager <- newManager $ defaultManagerSettings {
      managerConnCount = 5000
    , managerIdleConnectionCount = 5000
    , managerRawConnection = return $ openSocketConnectionSize (const $ return ()) goBufferSize
    }
  bracket (bindPortTCP (Warp.getPort settings) (Warp.getHost settings)) close $ \serverSocket -> do
    -- setInstallShutdownHandler settings close serverSocket
    let getConnection = do
            (clientSocket, clientSocketAddress) <- accept serverSocket
            setSocketCloseOnExec clientSocket
            setSocketOption clientSocket NoDelay 1
            connection <- do
              let
                bufferSize = goBufferSize + 4096
                sendAll = Sock.sendAll clientSocket
              bufferPool <- newBufferPool
              writeBuf <- allocateBuffer bufferSize
              return Connection {
                  connSendMany = Sock.sendMany clientSocket
                , connSendAll = sendAll
                , connSendFile = sendFile clientSocket writeBuf bufferSize sendAll
                , connRecv = receive clientSocket bufferPool
                , connClose = close clientSocket
                , connFree = freeBuffer writeBuf
                , connRecvBuf = receiveBuf clientSocket
                , connWriteBuffer = writeBuf
                , connBufferSize = bufferSize
                }
            return (connection, clientSocketAddress)
    runSettingsConnection settings getConnection $ proxyApp2 manager dest

proxyApp2 :: Manager -> WaiProxyResponse -> Application
proxyApp2 manager dest req res = do
  waiProxyTo (const $ return dest) defaultOnExc manager req res
