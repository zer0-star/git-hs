module Git.Object.Blob (createBlob) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as BB
import           Codec.Compression.Zlib
import           Data.Digest.Pure.SHA

createBlob :: FilePath -> IO (L.ByteString, Digest SHA1State)
createBlob path = do
  content <- L.readFile path
  let raw = rawBlob content
  return (compress raw, sha1 raw)

rawBlob :: L.ByteString -> L.ByteString
rawBlob content = BB.toLazyByteString
  $ "blob "
  <> BB.int64Dec (L.length content)
  <> "\0"
  <> BB.lazyByteString content

