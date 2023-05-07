module Command.Add where

import           Control.Monad
import           Control.Monad.Extra

import           Data.List

import           Flow

import           System.Directory
import           System.FilePath

import           Data.Git.Index
import           Data.Git.Object.Blob

runAdd :: [FilePath] -> Bool -> IO ()
runAdd paths dryRun = do
  paths' <- sort
    <$> concatMapM (dropTrailingPathSeparator .> flattenDirectory) paths
  if dryRun
    then forM_ paths' \path -> putStrLn $ "add " <> path
    else do
      files <- forM
        paths'
        \path -> do
          blob <- createBlob path
          writeBlob blob
          return (path, blob)
      updateIndexWith files

flattenDirectory :: FilePath -> IO [FilePath]
flattenDirectory path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  if
    | takeFileName path == ".git" -> return []
    | isDir -> do
      contents <- listDirectory path
      concatForM contents $ (path </>) .> flattenDirectory
    | isFile -> return [path]
    | otherwise -> fail "No such file or directory"
