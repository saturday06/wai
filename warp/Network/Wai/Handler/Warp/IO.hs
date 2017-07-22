{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.IO where

import Data.ByteString as BS (ByteString)
import Data.ByteString.Internal (ByteString(..))
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
    loop writer off = do
        (len, signal) <- writer (buf `plusPtr` off) $ size - off
        case signal of
             Done -> runIO $ off + len
             More minSize next
               | size < minSize -> error "toBufIOWith: BufferFull: minSize"
               | size - off >= minSize -> loop next $ off + len
               | otherwise      -> do
                   runIO $ off + len
                   loop next 0
             Chunk bs next
               | remaining >= bsLen -> do
                   _ <- copy (buf `plusPtr` (off + len)) bs
                   loop next $ off + len + bsLen
               | size >= bsLen  -> do
                   _ <- copy (buf `plusPtr` (off + len)) $ PS bsFp bsOff remaining
                   runIO size
                   _ <- copy buf $ PS bsFp (bsOff + remaining) $ bsLen - remaining
                   loop next $ bsLen + off + len - size
               | otherwise      -> do
                   runIO $ off + len
                   io bs
                   loop next 0
               where
                 remaining = size - off - len
                 (PS bsFp bsOff bsLen) = bs
