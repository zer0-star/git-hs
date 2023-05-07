module Data.Git.Index
    ( Index(..)
    , IndexEntry(..)
    , PaddedString(..)
    , updateIndexWith) where

import           Control.Monad

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString.Lazy as L
import           Data.Digest.Pure.SHA
import           Data.String
import           Data.Word

import           Flow

import           GHC.Generics

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
             , hash :: Digest SHA1State
             , fileName :: PaddedString
             }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (B.Binary)

newtype PaddedString = Padded { raw :: L.ByteString }
  deriving stock (Eq, Show, Ord)

instance B.Binary PaddedString where
  put (Padded bs) = do
    B.put @Word16 $ fromIntegral $ L.length bs
    B.putLazyByteString bs
    B.putLazyByteString $ L.replicate padLength 0
   where
    padLength = 8 - ((L.length bs + 6) `mod` 8)

  get = do
    len <- B.get @Word16
    let padLength = 8 - ((len + 6) `mod` 8)
    -- trace (show len) $ return ()
    -- trace (show padLength) $ return ()
    bs <- B.getLazyByteString $ fromIntegral len
    B.skip $ fromIntegral padLength
    return $ Padded bs

instance B.Binary Index where
  put (Index entries) = do
    B.putLazyByteString "DIRC"
    B.put @Word32 2
    B.put @Word32 $ fromIntegral $ length entries
    mapM_ B.put entries

  get = do
    B.skip 8
    numEntries <- B.get @Word32
    entries <- replicateM (fromIntegral numEntries) B.get
    B.skip 20
    return $ Index entries

constructIndexEntry :: L.ByteString -> Blob -> FileStatus -> IndexEntry
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
             , fileName = Padded path
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
    then B.decodeFile ".git/index"
    else return $ Index []

writeIndex :: Index -> IO ()
writeIndex index = do
  let bytes = B.encode index
  L.writeFile ".git/index" bytes

updateIndexWith :: [(FilePath, Blob)] -> IO ()
updateIndexWith files = do
  index <- getIndex
  entries <- mapM (uncurry createIndexEntry) files
  writeIndex $ insertEntries entries index