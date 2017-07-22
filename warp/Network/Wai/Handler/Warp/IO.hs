{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.IO where

import Data.ByteString as BS (ByteString, length)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder.Extra (runBuilder, Next(Done, More, Chunk))
import Foreign.Ptr (plusPtr)
import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.Types

toBufIOWith :: Buffer -> BufSize -> (ByteString -> IO ()) -> Builder -> IO ()
toBufIOWith buf !size io builder = loop firstWriter 0
  where
    firstWriter = runBuilder builder
    runIO len = bufferIO buf len io
    loop writer writtenSize = do
        (len, signal) <- writer (buf `plusPtr` writtenSize) $ size - writtenSize
        case signal of
             Done -> runIO $ writtenSize + len
             More minSize next
               | size < minSize -> error "toBufIOWith: BufferFull: minSize"
               | size - writtenSize >= minSize -> loop next $ writtenSize + len
               | otherwise      -> do
                   runIO $ writtenSize + len
                   loop next 0
             Chunk bs next
               | len == 0 && bsLen == 0 -> loop next writtenSize
               | size - writtenSize - len >= bsLen -> do
                   _ <- copy (buf `plusPtr` (writtenSize + len)) bs
                   loop next $ writtenSize + len + bsLen
               | size >= bsLen  -> do
                   runIO $ writtenSize + len
                   _ <- copy buf bs
                   loop next bsLen
               | otherwise      -> do
                   runIO $ writtenSize + len
                   io bs
                   loop next 0
               where
                 bsLen = BS.length bs
