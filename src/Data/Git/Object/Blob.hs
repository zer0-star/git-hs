module Data.Git.Object.Blob (createBlob, Blob(..), writeBlob) where

import           Codec.Compression.Zlib

import qualified Crypto.Hash.SHA1 as SHA1

import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BB

import           Flow

import           System.Directory
import           System.FilePath

import           Text.Bytedump

data Blob = Blob { content :: ByteString, hash :: ByteString }
  deriving (Eq)

createBlob :: FilePath -> IO Blob
createBlob path = do
  content <- BS.readFile path
  let raw = rawBlob content
  return
    $ Blob (raw |> BS.fromStrict |> compress |> BS.toStrict) (SHA1.hash raw)

rawBlob :: ByteString -> ByteString
rawBlob content = BS.toStrict
  $ BB.toLazyByteString
  $ "blob " <> BB.intDec (BS.length content) <> "\0" <> BB.byteString content

writeBlob :: Blob -> IO ()
writeBlob (Blob content hash) = do
  exists <- doesFileExist path
  if exists
    then return ()
    else do
      createDirectoryIfMissing True $ takeDirectory path
      BS.writeFile path content
 where
  (h, t) = splitAt 2 $ dumpRawBS hash

  path = ".git" </> "objects" </> h </> t