{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import Assertions (assertAllStaticRefsExist)
import Config (staticSrc, targetDir)
import Control.Monad (forM_)
import Data.List (nub)
import Data.Map as Map (elems, traverseWithKey)
import Data.Text.Lazy.IO as TLIO (writeFile)
import Lucid (renderText)
import LucidUtils (HTML, expandPath)
import Options.Applicative
import PageSpecs (loadBlog)
import System.Directory (copyFile, createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath (takeFileName, (</>))
import Templates
  ( Blog (..),
    Post (staticPaths, tags),
    Posts,
    aboutHtml,
    allTagsHtml,
    assemblePost,
    contactHtml,
    homeHtml,
    postsHtml,
    publicationsHtml,
    tagMatchHtml,
    teachingHtml,
    upcomingHtml,
  )

data CliArgs = CliArgs
  { devMode :: Bool,
    noComments :: Bool
  }

main :: IO ()
main = do
  CliArgs {devMode, noComments} <- execParser optsInfo

  putStrLn $
    if devMode
      then "Running in Development Mode"
      else "Running in Production Mode"

  putStrLn $
    if noComments
      then "Without Comments"
      else "With Comments"

  blog <- loadBlog devMode noComments

  putStrLn "Creating html directory."
  generateStaticSite blog

  putStrLn "Checking all static references exist."
  assertAllStaticRefsExist "html"
  where
    optsInfo :: ParserInfo CliArgs
    optsInfo =
      info
        (optionsParser <**> helper)
        fullDesc

    optionsParser :: Parser CliArgs
    optionsParser =
      CliArgs
        <$> switch (long "dev")
        <*> switch (long "no-comments")

generateStaticSite :: Blog -> IO ()
generateStaticSite Blog {home, about, upcoming, contact, publications, teaching, posts} = do
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
  htmlToDir (teachingHtml teaching) $ targetDir </> "teaching"
  Map.traverseWithKey postToFile posts
  mapM_ tagToFile $ distinctTags posts -- page per tag listing posts with that tag
  where
    tagToFile tag = htmlToDir (tagMatchHtml posts tag) $ targetDir </> "tags" </> tag
    postToFile endpoint post = createHtmlDir (assemblePost post) (staticPaths post) (targetDir </> "posts" </> endpoint)

htmlToDir :: HTML -> FilePath -> IO ()
htmlToDir h = createHtmlDir h []

createHtmlDir :: HTML -> [FilePath] -> FilePath -> IO ()
createHtmlDir html extraStaticPaths htmlDir = do
  safeCreateDir htmlDir
  TLIO.writeFile (htmlDir </> "index.html") (renderText html)
  safeCreateDir $ htmlDir </> "static"
  copyFile (staticSrc </> "favicon.ico") (htmlDir </> "static" </> "favicon.ico")
  forM_ extraStaticPaths $ \path ->
    copyFile path (htmlDir </> "static" </> takeFileName path)

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