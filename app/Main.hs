{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import Config (staticSrc, targetDir)
import Control.Monad (forM_)
import Data.List (nub)
import Data.Map as Map (elems, traverseWithKey)
import Data.Text.Lazy.IO as TLIO (writeFile)
import Lucid (renderText)
import LucidUtils (HTML, expandPath)
import PageSpecs (loadBlog)
import System.Directory (copyFile, createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>), takeFileName)
import Templates
  ( Blog (..),
    Post (tags, staticPaths),
    Posts,
    aboutHtml,
    allTagsHtml,
    contactHtml,
    homeHtml,
    assemblePost,
    postsHtml,
    publicationsHtml,
    tagMatchHtml,
    upcomingHtml,
  )

main :: IO ()
main = do
#ifdef DEV_MODE
  -- -fdev flag see: .cabal file
  putStrLn "Running in Development Mode"
  let devMode = True
#else
  putStrLn "Running in Production Mode"
  let devMode = False
#endif
#ifdef NO_COMMENTS
  -- -fdev flag see: .cabal file
  putStrLn "Without Comments"
  let noComments = True
#else
  putStrLn "With Comments"
  let noComments = False
#endif
  blog <- loadBlog devMode noComments
  generateStaticSite blog

generateStaticSite :: Blog -> IO ()
generateStaticSite Blog {home, about, upcoming, contact, publications, posts} = do
  putStrLn $ "Deleting: " ++ targetDir ++ " directory"
  removeDirectoryRecursive targetDir
  safeCreateDir targetDir
  safeCreateDir $ targetDir </> "tags"
  safeCreateDir $ targetDir </> "posts"
  -- Copy existing static resources (e.g. .css, .js files)
  copyDir staticSrc targetDir
  -- Generate new static resources
  htmlToDir (homeHtml home posts) targetDir
  htmlToDir (postsHtml posts) $ targetDir </> "posts"
  htmlToDir (allTagsHtml posts) $ targetDir </> "tags"
  htmlToDir (aboutHtml about) $ targetDir </> "about"
  htmlToDir (upcomingHtml upcoming) $ targetDir </> "upcoming"
  htmlToDir (contactHtml contact) $ targetDir </> "contact"
  htmlToDir (publicationsHtml publications) $ targetDir </> "publications"
  Map.traverseWithKey postToFile posts
  mapM_ tagToFile $ distinctTags posts -- page per tag listing posts with that tag
  where
    tagToFile tag = htmlToDir (tagMatchHtml posts tag) $ targetDir </> "tags" </> tag
    postToFile endpoint post = createHtmlDir (assemblePost post) (staticPaths post) (targetDir </> "posts" </> endpoint)

htmlToDir :: HTML -> FilePath -> IO ()
htmlToDir h d =  createHtmlDir h [] d

createHtmlDir :: HTML -> [FilePath] -> FilePath -> IO ()
createHtmlDir html extraStaticPaths htmlDir = do
  safeCreateDir htmlDir
  TLIO.writeFile (htmlDir </> "index.html") (renderText html)
  safeCreateDir $ htmlDir </> "static"
  copyFile (staticSrc </> "favicon.ico") (htmlDir </> "static" </> "favicon.ico")
  forM_ extraStaticPaths $ \path ->
      copyFile (path) (htmlDir </> "static" </> takeFileName path)

distinctTags :: Posts -> [String]
distinctTags posts = nub . concat $ tagLists -- merge tag lists and dedupe
  where
    values = Map.elems posts
    tagLists = map tags values -- list of tags per page

copyDir :: FilePath -> FilePath -> IO ()
copyDir src target = do
  safeCreateDir $ target </> src
  toCopy <- expandPath src
  mapM_ cp toCopy
  where
    cp path = copyFile path $ target </> path

safeCreateDir :: FilePath -> IO ()
safeCreateDir = createDirectoryIfMissing True