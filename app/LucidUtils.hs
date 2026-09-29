module LucidUtils (markdownToLucid, renderPathsOrdered, expandPath, HTML, renderPath, latexToLucid, renderText) where

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
  let ext = takeExtension path
  if ext == ".png"
    then return (makeImg $ pack path)
    else TIO.readFile path >>= renderText ext

renderText :: String -> Text -> IO HTML
renderText ".md"   = markdownToLucid
renderText ".html" = htmlToLucid
renderText ".svg"  = htmlToLucid
renderText ".tex"  = latexToLucid
renderText ext     = \_ -> error $ "Missing implementation for " ++ ext ++ " to HTML"

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
-- Note: it's hard to tell what kind of latex this supports.
-- The most comprehensive support comes from copying snippets via https://upmath.me/ to .md files

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