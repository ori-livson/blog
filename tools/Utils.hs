module Utils (getInnerDirs, getInnerFiles, getSectionDir, getStaticDir, getPostDirs, safeCreateDir, getFootnotesDir) where

import Control.Monad (filterM)
import Data.List (sort)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

getPostDirs :: IO [FilePath]
getPostDirs = getInnerDirs $ "content" </> "posts"

getSectionDir :: FilePath -> IO FilePath
getSectionDir postDir = do
  let bodyDir = postDir </> "body"
  exists <- doesDirectoryExist bodyDir
  return $ (if exists then bodyDir else postDir)

getStaticDir :: FilePath -> IO (Maybe FilePath)
getStaticDir sectionDir = do
  let staticDir = sectionDir </> "static"
  exists <- doesDirectoryExist staticDir
  return $ if exists then Just staticDir else Nothing

getFootnotesDir :: FilePath -> IO (Maybe FilePath)
getFootnotesDir postDir = do
  let footnotesDir = postDir </> "footnotes"
  exists <- doesDirectoryExist footnotesDir
  return $ if exists then Just footnotesDir else Nothing

-- Subdir listing

getInnerDirs :: FilePath -> IO [FilePath]
getInnerDirs dir = sort <$> (filterM doesDirectoryExist =<< innerPaths dir)

getInnerFiles :: FilePath -> IO [FilePath]
getInnerFiles dir = do
  sort <$> (filterM doesFileExist =<< innerPaths dir)

innerPaths :: FilePath -> IO [FilePath]
innerPaths dir = map (dir </>) <$> listDirectory dir

safeCreateDir :: FilePath -> IO ()
safeCreateDir = createDirectoryIfMissing True