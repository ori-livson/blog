module Assertions (assertAllStaticRefsExist) where

import Control.Monad (unless)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, nub)
import Network.URI (unEscapeString)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (exitFailure)
import System.FilePath (normalise, takeDirectory, (</>))
import Text.HTML.TagSoup (fromAttrib, isTagOpen, parseTags)

-- For every .html file found recursively in htmlDir:
--    Assert every static refs (e.g., src, href) corresponds to a real file
assertAllStaticRefsExist :: FilePath -> IO ()
assertAllStaticRefsExist htmlDir = do
  htmlFiles <- findHtmlFiles htmlDir
  mapM_ (assertStaticRefsExist htmlDir) htmlFiles

assertStaticRefsExist :: FilePath -> FilePath -> IO ()
assertStaticRefsExist htmlDir htmlFile = do
  html <- readFile htmlFile
  let refs = nub (extractStaticRefs html) -- nub dedupes lists
  missing <- mapM (checkRef htmlDir) refs
  unless (and missing) exitFailure
  where
    checkRef :: FilePath -> String -> IO Bool
    checkRef root ref = do
      -- Check if ref is absolute (i.e., /static/... ) or relative (i.e., static/...)
      let decoded = unEscapeString ref
          absPath = normalise (root </> toRelative decoded)

          relBase =
            if "static" `isSuffixOf` takeDirectory htmlFile
              then takeDirectory (takeDirectory htmlFile)
              else takeDirectory htmlFile
          relPath = normalise (relBase </> decoded)

          isAbsRef = "/" `isPrefixOf` ref

      absExists <- doesFileExist absPath
      relExists <- doesFileExist relPath
      let exists = if isAbsRef then absExists else absExists || relExists

      unless exists $
        putStrLn $
          htmlFile
            <> " has missing static ref: "
            <> ref
            <> if isAbsRef
              then " (looked at: " <> absPath <> ")"
              else " (looked at: " <> absPath <> " and " <> relPath <> ")"
      return exists

findHtmlFiles :: FilePath -> IO [FilePath]
findHtmlFiles dir = do
  entries <- listDirectory dir
  foldMap (classify dir) entries
  where
    classify root name = do
      let path = root </> name
      isDir <- doesDirectoryExist path
      if isDir
        then findHtmlFiles path
        else return [path | ".html" `isSuffixOf` name]

extractStaticRefs :: String -> [String]
extractStaticRefs html =
  [ v
    | tag <- parseTags html,
      isTagOpen tag,
      name <- ["src", "href"],
      let v = fromAttrib name tag,
      "static" `isInfixOf` v,
      not (null v),
      not ("https" `isPrefixOf` v)
  ]

-- Strip a leading "/" to make the path relative for joining.
toRelative :: FilePath -> FilePath
toRelative ('/' : rest) = rest
toRelative path = path