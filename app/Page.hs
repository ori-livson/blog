module Page (Page (..), Pages, textToLucid, loadPathsOrdered, expandPath, LucidHtml, loadPath) where

import Cheapskate (def, markdown)
import Cheapskate.Lucid (renderDoc)
import Control.Monad ((<=<))
import Data.Binary.Builder (toLazyByteString)
import Data.List (sort)
import Data.Map (Map)
import Data.Text (Text, pack)
import Data.Time.Calendar (Day)
import Lucid (Html, HtmlT (..), ToHtml (toHtmlRaw))
import System.Directory (doesDirectoryExist, doesPathExist)
import System.Directory.Recursive (getFilesRecursive)
import System.FilePath (takeExtension)

type LucidHtml = Html ()

-- Pages

data Page = Page
  { title :: String,
    subtitle :: Maybe String,
    date :: Day,
    tags :: [String],
    body :: [(Maybe String, LucidHtml)],
    footnotes :: [LucidHtml],
    comments :: [LucidHtml],
    issueId :: Int
  }

type Pages = Map String Page

loadPathsOrdered :: FilePath -> IO [LucidHtml]
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

loadPath :: FilePath -> IO LucidHtml
loadPath path = do
  text <- pack <$> readFile path
  case takeExtension path of
    ".md" -> textToLucid text
    unknown -> error $ "Missing implementation for " ++ unknown ++ " to HTML"

textToLucid :: Text -> IO LucidHtml
textToLucid text = do
  builder <- apply mempty . fst <$> runHtmlT (textToLucidT text)
  return $ toHtmlRaw $ toLazyByteString builder

textToLucidT :: Text -> HtmlT IO ()
textToLucidT text = do
  let doc = markdown def text
  renderDoc doc

-- Helpers

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c t f = c >>= (\c' -> if c' then t else f)

apply :: a -> (a -> b) -> b
apply = flip ($)