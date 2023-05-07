module Data.Git.Object.Blob (createBlob, Blob(..), writeBlob) where

import           Codec.Compression.Zlib

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as L
import           Data.Digest.Pure.SHA

import           System.Directory
import           System.FilePath

data Blob = Blob { content :: L.ByteString, hash :: Digest SHA1State }
  deriving (Eq)

createBlob :: FilePath -> IO Blob
createBlob path = do
  content <- L.readFile path
  let raw = rawBlob content
  return $ Blob (compress raw) (sha1 raw)

rawBlob :: L.ByteString -> L.ByteString
rawBlob content = BB.toLazyByteString
  $ "blob "
  <> BB.int64Dec (L.length content)
  <> "\0"
  <> BB.lazyByteString content

writeBlob :: Blob -> IO ()
writeBlob (Blob content hash) = do
  exists <- doesFileExist path
  if exists
    then return ()
    else do
      createDirectoryIfMissing True $ takeDirectory path
      L.writeFile path content
 where
  (h, t) = splitAt 2 $ show hash

  path = ".git" </> "objects" </> h </> t