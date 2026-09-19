module LucidUtils (markdownToLucid, renderPathsOrdered, expandPath, HTML, renderPath, latexToLucid) where

import Control.Monad ((<=<))
import Data.List (sort)
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Lucid (Html, ToHtml (toHtmlRaw), img_, src_)
import System.Directory (doesDirectoryExist, doesPathExist)
import System.Directory.Recursive (getFilesRecursive)
import System.FilePath (takeExtension)
import Text.Pandoc

type HTML = Html ()

-- Posts

renderPathsOrdered :: FilePath -> IO [HTML]
renderPathsOrdered = pathsToHtml <=< expandPathSorted
  where
    expandPathSorted = (return . sort) <=< expandPath
    pathsToHtml = mapM renderPath

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

renderPath :: FilePath -> IO HTML
renderPath path = do
  case takeExtension path of
    ".md" -> contents >>= markdownToLucid
    ".html" -> contents >>= htmlToLucid
    ".svg" -> contents >>= htmlToLucid
    ".tex" -> contents >>= latexToLucid
    ".png" -> return . makeImg . pack $ path
    unknown -> error $ "Missing implementation for " ++ unknown ++ " to HTML"
  where
    contents = TIO.readFile path

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
  runPure $ writeHtml5String def {writerMathMethod = MathJax ""} pandoc

makeImg :: Text -> HTML
makeImg src = img_ [src_ src]