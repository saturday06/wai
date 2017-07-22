{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blaze.ByteString.Builder.HTTP
import Control.Exception
import Criterion.Main
import Data.ByteString.Builder
import Data.Traversable
import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.IO
import Network.Wai.Handler.Warp.Types
import System.IO
import System.Info
import qualified Data.ByteString as S

-- $setup
-- >>> :set -XOverloadedStrings

nullFilePath :: FilePath
nullFilePath = if os == "mingw" then "nul" else "/dev/null"

xBuilder :: Int -> Builder
xBuilder size = stringUtf8 $ replicate size 'x'

benchToBufIOWith :: Buffer -> (S.ByteString -> IO ()) -> Int -> Benchmark
benchToBufIOWith buffer io size = bgroup (show size ++ " bytes") [
    bench "toBufIOWith " $ whnfIO $ toBufIOWith buffer bufferSize io $ xBuilder size
  , bench "toBufIOWith Chunked" $ whnfIO $ toBufIOWith buffer bufferSize io $ chunkedTransferEncoding $ xBuilder size
  ]

main :: IO ()
main = do
  withFile nullFilePath WriteMode $ \nullFile -> do
    let io = S.hPut nullFile
    bracket (allocateBuffer bufferSize) freeBuffer $ \buffer -> do
      defaultMain [
          benchToBufIOWith buffer io 1024
        , benchToBufIOWith buffer io $ 1024 * 1024
        , benchToBufIOWith buffer io $ 50 * 1024 * 1024
        ]
