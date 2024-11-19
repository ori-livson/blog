{-# LANGUAGE CPP #-}
module Main (main) where

import Config (staticSrc, targetDir)
import Data.List (nub)
import Data.Map as Map (elems, keys)
import Data.Text.Lazy.IO as TLIO (writeFile)
import Lucid (renderText)
import LucidUtils (LucidHtml, expandPath)
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath ((</>))
import Templates (aboutHtml, allTagsHtml, Blog(..), contactHtml, homeHtml, postsHtml, postHtml, publicationsHtml, tagMatchHtml, Post (tags), Posts)
import PageSpecs (loadBlog)


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
generateStaticSite Blog {home, about, contact, publications, posts} = do
  safeCreateDir targetDir
  safeCreateDir $ targetDir </> "tags"
  safeCreateDir $ targetDir </> "posts"
  -- Copy existing static resources (e.g. .css, .js files)
  copyDir staticSrc targetDir
  -- Generate new static resources
  htmlToFile (homeHtml home posts) targetDir
  htmlToFile (postsHtml posts) $ targetDir </> "posts"
  htmlToFile (allTagsHtml posts) $ targetDir </> "tags"
  htmlToFile (aboutHtml about) $ targetDir </> "about"
  htmlToFile (contactHtml contact) $ targetDir </> "contact"
  htmlToFile (publicationsHtml publications) $ targetDir </> "publications"
  mapM_ postToFile $ Map.keys posts
  mapM_ tagToFile $ distinctTags posts -- page per tag listing posts with that tag
  where
    tagToFile tag = htmlToFile (tagMatchHtml posts tag) $ targetDir </> "tags" </> tag
    postToFile endpoint = htmlToFile (postHtml posts endpoint) $ targetDir </> "posts" </> endpoint

htmlToFile :: LucidHtml -> FilePath -> IO ()
htmlToFile html htmlDir = do
  safeCreateDir htmlDir
  TLIO.writeFile (htmlDir </> "index.html") (renderText html)
  safeCreateDir $ htmlDir </> "static"
  copyFile (staticSrc </> "favicon.ico") (htmlDir </> "static" </> "favicon.ico")


distinctTags :: Posts -> [String]
distinctTags posts = nub . concat $ tagLists  -- merge tag lists and dedupe
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