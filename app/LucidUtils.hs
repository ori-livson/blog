module LucidUtils (markdownToLucid, loadPathsOrdered, expandPath, HTML, loadPath, latexToLucid) where

import Control.Monad ((<=<))
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Lucid (Html, ToHtml (toHtmlRaw))
import System.Directory (doesDirectoryExist, doesPathExist)
import System.Directory.Recursive (getFilesRecursive)
import System.FilePath (takeExtension)
import Text.Pandoc

type HTML = Html ()

-- Posts

loadPathsOrdered :: FilePath -> IO [HTML]
loadPathsOrdered = pathsToHtml <=< expandPathSorted
  where
    expandPathSorted = (return . sort) <=< expandPath
    pathsToHtml = mapM loadPath

expandPath :: FilePath -> IO [FilePath]
expandPath path =
  ifM
    (doesPathExist path)
    ( ifM
        (doesDirectoryExist path)
        (getFilesRecursive path)
        (return [path])
    )
    (error $ path ++ " Does not exist!")
  where
    ifM :: (Monad m) => m Bool -> m a -> m a -> m a
    ifM c t f = c >>= (\c' -> if c' then t else f)

loadPath :: FilePath -> IO HTML
loadPath path = do
  text <- TIO.readFile path
  case takeExtension path of
    ".md" -> markdownToLucid text
    ".html" -> htmlToLucid text
    ".svg" -> htmlToLucid text
    ".tex" -> latexToLucid text
    unknown -> error $ "Missing implementation for " ++ unknown ++ " to HTML"

-- Markdown Reading through Pandoc

markdownToLucid :: Text -> IO HTML
markdownToLucid text = do
  case markdownToHtmlText text of
    Left err -> error $ "Markdown conversion failed: " ++ show err
    Right htmlContent -> return $ toHtmlRaw htmlContent

markdownToHtmlText :: Text -> Either PandocError Text
markdownToHtmlText markdownInput = do
  let readerOptions = def {readerExtensions = pandocExtensions}
  pandoc <- runPure $ readMarkdown readerOptions markdownInput
  runPure $ writeHtml5String def pandoc

-- HTML Reading through Lucid

htmlToLucid :: Text -> IO HTML
htmlToLucid text = do
  return $ toHtmlRaw text

-- Latex Reading through Pandoc
-- Note: this doesn't work very well
-- I prefer MathJax in markdown, and https://upmath.me/) for things like tikzcd.

latexToLucid :: Text -> IO HTML
latexToLucid text = do
  case latexToHtmlText text of
    Left err -> error $ "Latex conversion failed: " ++ show err
    Right htmlContent -> return $ toHtmlRaw htmlContent

latexToHtmlText :: Text -> Either PandocError Text
latexToHtmlText latexInput = do
  let readerOptions = def {readerExtensions = enableExtension Ext_latex_macros (readerExtensions def)}
  pandoc <- runPure $ readLaTeX readerOptions latexInput
  runPure $ writeHtml5String def {writerHTMLMathMethod = MathJax ""} pandoc
