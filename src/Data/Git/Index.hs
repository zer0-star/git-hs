module Data.Git.Index (Index(..), IndexEntry(..), updateIndexWith) where

import           Control.Monad

import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Data.Bytes.Get as B
import qualified Data.Bytes.Put as B
import qualified Data.Bytes.Serial as B
import           Data.Bytes.Serial (Serial(deserialize))
import           Data.String
import           Data.Word

import           Flow

import           System.Posix.Files.ByteString

import           Data.Git.Object.Blob

newtype Index = Index [IndexEntry]
  deriving (Eq, Show)

data IndexEntry =
  IndexEntry { ctimeSeconds :: Word32
             , ctimeNanosecondFraction :: Word32
             , mtimeSeconds :: Word32
             , mtimeNanosecondFraction :: Word32
             , dev :: Word32
             , ino :: Word32
             , mode :: Word32
             , uid :: Word32
             , gid :: Word32
             , size :: Word32
             , hash :: ByteString
             , fileName :: ByteString
             }
  deriving stock (Eq, Show)

instance B.Serial IndexEntry where
  serialize IndexEntry {..} = do
    B.putWord32be ctimeSeconds
    B.putWord32be ctimeNanosecondFraction
    B.putWord32be mtimeSeconds
    B.putWord32be mtimeNanosecondFraction
    B.putWord32be dev
    B.putWord32be ino
    B.putWord32be mode
    B.putWord32be uid
    B.putWord32be gid
    B.putWord32be size
    B.putByteString hash
    putFileName fileName
   where
    putFileName bs = do
      B.putWord16be $ fromIntegral $ BS.length bs
      B.putByteString bs
      B.putByteString $ BS.replicate (padLength bs) 0

    padLength bs = 8 - ((BS.length bs + 6) `mod` 8)

  deserialize = do
    ctimeSeconds <- B.getWord32be
    ctimeNanosecondFraction <- B.getWord32be
    mtimeSeconds <- B.getWord32be
    mtimeNanosecondFraction <- B.getWord32be
    dev <- B.getWord32be
    ino <- B.getWord32be
    mode <- B.getWord32be
    uid <- B.getWord32be
    gid <- B.getWord32be
    size <- B.getWord32be
    hash <- B.getByteString 20
    fileName <- getFileName
    return IndexEntry {..}
   where
    getFileName = do
      len <- B.getWord16be
      let padLength = 8 - ((len + 6) `mod` 8)
      -- trace (show len) $ return ()
      -- trace (show padLength) $ return ()
      bs <- B.getByteString $ fromIntegral len
      B.skip $ fromIntegral padLength
      return bs

instance B.Serial Index where
  serialize (Index entries) = do
    B.putLazyByteString "DIRC"
    B.putWord32be 2
    B.putWord32be $ fromIntegral $ length entries
    mapM_ B.serialize entries

  deserialize = do
    B.skip 8
    numEntries <- B.getWord32be
    entries <- replicateM (fromIntegral numEntries) B.deserialize
    -- B.skip 20
    return $ Index entries

constructIndexEntry :: ByteString -> Blob -> FileStatus -> IndexEntry
constructIndexEntry path blob stat =
  IndexEntry { ctimeSeconds
             , ctimeNanosecondFraction = floor $ ctimeFraction * 1e9
             , mtimeSeconds
             , mtimeNanosecondFraction = floor $ mtimeFraction * 1e9
             , dev = fromIntegral $ deviceID stat
             , ino = fromIntegral $ fileID stat
             , mode =
                 fromIntegral $ intersectFileModes 0o777755 $ fileMode stat
             , uid = fromIntegral $ fileOwner stat
             , gid = fromIntegral $ fileGroup stat
             , size = fromIntegral $ fileSize stat
             , hash = blob.hash
             , fileName = path
             }
 where
  (ctimeSeconds, ctimeFraction) = properFraction $ statusChangeTimeHiRes stat

  (mtimeSeconds, mtimeFraction) = properFraction $ modificationTimeHiRes stat

insertEntries :: [IndexEntry] -> Index -> Index
insertEntries entries (Index oldEntries) = Index newEntries
 where
  newEntries = merge entries oldEntries id

  merge [] ys f = f ys
  merge xs [] f = f xs
  merge (x:xs) (y:ys) f = case compare x.fileName y.fileName of
    LT -> merge xs (y:ys) $ (x:) .> f
    EQ -> merge xs ys $ (x:) .> f
    GT -> merge (x:xs) ys $ (y:) .> f

createIndexEntry :: FilePath -> Blob -> IO IndexEntry
createIndexEntry path blob = do
  stat <- getFileStatus (fromString path)
  return $ constructIndexEntry (fromString path) blob stat

getIndex :: IO Index
getIndex = do
  exists <- fileExist ".git/index"
  if exists
    then do
      result <- B.runGetS B.deserialize <$> BS.readFile ".git/index"
      case result of
        Left err    -> error err
        Right index -> return index
    else return $ Index []

writeIndex :: Index -> IO ()
writeIndex index = do
  let bytes = B.runPutS $ B.serialize index
  BS.writeFile ".git/index" bytes

updateIndexWith :: [(FilePath, Blob)] -> IO ()
updateIndexWith files = do
  index <- getIndex
  entries <- mapM (uncurry createIndexEntry) files
  writeIndex $ insertEntries entries index