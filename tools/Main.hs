module Main where

import Control.Monad (filterM)
import Data.Char (toUpper)
import Data.List (intercalate, sort)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (dropExtension, takeExtension, takeFileName, (</>))

-- Go into each posts folder and for each file with relative path <f> print
-- <var f> <- renderAbs| "<f>"
-- [Just "<title f>", <var f>]
--
-- Where <var f> converts expressions like "0-serious-introduction.md" to "seriousIntroduction"
-- Also handles all static resources as figures as
-- <var f> <- makeFigure "80%" "Figure X:" <$> <renderAbs|renderPath> ("static" </> <f>)
-- (Nothing,        , <figure f>) for f in staticFnames
main :: IO ()
main = do
  -- One dir f per post
  postDirs <- getInnerDirs $ "content" </> "posts"
  -- Still 1 dir per post but may be < f / body >
  sectionDirs <- mapM getSectionDir postDirs
  mapM_ displaySections sectionDirs

getSectionDir :: FilePath -> IO FilePath
getSectionDir dir = do
  let bodyDir = dir </> "body"
  exists <- doesDirectoryExist bodyDir
  return $ (if exists then bodyDir else dir)

displaySections :: FilePath -> IO ()
displaySections sectionDir = do
  putStrLn "================================================"
  putStrLn sectionDir
  putStrLn "================================================"
  files <- getInnerFiles sectionDir
  let fnames = map takeFileName files

  staticDir <- getStaticDir sectionDir
  staticPaths <- maybe (return []) getInnerFiles staticDir
  let staticFnames = map takeFileName staticPaths

  -- <var f> <- renderAbs| "<f>"
  mapM_ (putStrLn . renderStatement) fnames

  -- <var f> <- makeFigure "80%" "Figure X:" <$> <renderAbs|renderPath> ("static" </> <f>)
  mapM_ (putStrLn . renderStaticStatement) staticFnames

  -- (Just "<title f>", <var f>) for f in fnames
  -- (Nothing,        , <figure f>) for f in staticFnames
  let sections = (map sectionStatement fnames) ++ (map staticSectionStatement staticFnames)

  putStrLn $ ("let body = " ++ listStr sections)
  case staticDir of
    Just _ -> do
      putStrLn "Don't forget to add:"
      putStrLn "allStaticPaths <- listDirectoryRecursive $ bodyDir </> \"static\""
      putStrLn "..."
      putStrLn "staticPaths = allStaticPaths,"
    Nothing -> return ()
  where
    renderStatement fname = (var fname) ++ " <- renderAbs " ++ (quote fname)
    -- Make (Maybe Heading, html) entrires for a post "body" folder
    sectionStatement fname = tupleStr ["Just " ++ heading fname, var fname]

    renderStaticStatement fname =
      (var fname)
        ++ " <- "
        ++ unwords
          [ "makeFigure",
            quote "80%",
            quote "Figure X:",
            "<$>",
            loadMethod fname, -- (renderAbs | renderPath)
            relPath fname -- ("static" </> <f>)
          ]
    loadMethod fname = if takeExtension fname == ".png" then "renderPath" else "renderAbs"
    relPath fname = "( " ++ quote "static" ++ " </> " ++ quote fname ++ " )"
    staticSectionStatement fname = tupleStr ["Nothing", var fname]

    -- transform a/b/c/0-intro-x.md to "Intro X"
    heading = quote . kebabToTitle . dropFnameMeta

    -- transform a/b/c/0-intro-x.md to
    -- introX <- load "0-intro-x.md"
    var = kebabToCamel . dropFnameMeta

getStaticDir :: FilePath -> IO (Maybe FilePath)
getStaticDir sectionDir = do
  let staticDir = sectionDir </> "static"
  exists <- doesDirectoryExist staticDir
  return $ if exists then Just staticDir else Nothing

-- Drop file number prefix and extension
dropFnameMeta :: String -> String
dropFnameMeta xs =
  case break (== '-') xs of
    (_, '-' : rest) -> dropExtension rest
    _ -> dropExtension xs

-- Subdir listing

getInnerDirs :: FilePath -> IO [FilePath]
getInnerDirs dir = sort <$> (filterM doesDirectoryExist =<< innerPaths dir)

getInnerFiles :: FilePath -> IO [FilePath]
getInnerFiles dir = do
  sort <$> (filterM doesFileExist =<< innerPaths dir)

innerPaths :: FilePath -> IO [FilePath]
innerPaths dir = map (dir </>) <$> listDirectory dir

-- String format conversions

-- "hello-world" -> "helloWorld"
-- Note kebabSplit always returns a populated list
--  (i.e., at least a singleton with the whole string)
kebabToCamel :: String -> String
kebabToCamel "" = ""
kebabToCamel s =
  let parts = kebabSplit s
   in head parts ++ concatMap capitalise (tail parts)

-- "hello-world" -> "Hello World"
kebabToTitle :: String -> String
kebabToTitle = unwords . map capitalise . kebabSplit

kebabSplit :: String -> [String]
kebabSplit xs =
  case break (== '-') xs of
    (before, '-' : rest) -> [before] ++ kebabSplit rest
    _ -> [xs]

capitalise :: String -> String
capitalise (x : xs) = toUpper x : xs
capitalise "" = ""

-- Pretty string representations of Haskell types

quote :: String -> String
quote s = "\"" ++ s ++ "\""

tupleStr :: [String] -> String
tupleStr lst = "(" ++ intercalate ", " lst ++ ")"

listStr :: [String] -> String
listStr lst = "\n\t[ " ++ intercalate ",\n\t" lst ++ "\n\t]"