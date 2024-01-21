module Main (main) where

import Data.List (nub)
import Data.Map as Map (elems, keys)
import Data.Text.Lazy.IO as TLIO (writeFile)
import Lucid (renderText)
import Page (LucidHtml, Page (tags), Pages, expandPath)
import PageSpecs (loadPages)
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath ((</>))
import Templates (contactHtml, homeHtml, postHtml, tagsHtml)

main :: IO ()
main = do
  pages <- loadPages
  generateStaticSite pages

generateStaticSite :: Pages -> IO ()
generateStaticSite pages = do
  safeCreateDir staticRoot
  safeCreateDir $ staticRoot </> "tags"
  safeCreateDir $ staticRoot </> "posts"
  copyDir "static" staticRoot
  htmlToFile (homeHtml pages) staticRoot
  htmlToFile contactHtml $ staticRoot </> "contact"
  mapM_ postToFile $ Map.keys pages
  mapM_ tagToFile $ distinctTags pages
  where
    tagToFile tag = htmlToFile (tagsHtml pages tag) $ staticRoot </> "tags" </> tag
    postToFile endpoint = htmlToFile (postHtml pages endpoint) $ staticRoot </> "posts" </> endpoint
    staticRoot = "html"

htmlToFile :: LucidHtml -> FilePath -> IO ()
htmlToFile html htmlDir = do
  safeCreateDir htmlDir
  TLIO.writeFile (htmlDir </> "index.html") (renderText html)

distinctTags :: Pages -> [String]
distinctTags pages = concat . nub $ tagLists
  where
    values = Map.elems pages
    tagLists = map tags values

copyDir :: FilePath -> FilePath -> IO ()
copyDir src target = do
  safeCreateDir $ target </> src
  toCopy <- expandPath src
  mapM_ cp toCopy
  where
    cp path = copyFile path $ target </> path

safeCreateDir :: FilePath -> IO ()
safeCreateDir = createDirectoryIfMissing True