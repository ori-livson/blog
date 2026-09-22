module PageSpecGenerator (printSpecs) where

import Data.Char (toUpper)
import Data.List (intercalate)
import System.FilePath (dropExtension, takeExtension, takeFileName)
import Utils (getInnerFiles, getPostDirs, getSectionDir, getStaticDir)

-- Go into each posts folder and for each file with relative path <f> print
-- <var f> <- renderAbs| "<f>"
-- [Just "<title f>", <var f>]
--
-- Where <var f> converts expressions like "0-serious-introduction.md" to "seriousIntroduction"
-- Also handles all static resources as figures as
-- <var f> <- makeFigure "80%" "Figure X:" <$> <renderAbs|renderPath> ("static" </> <f>)
-- (Nothing,        , <figure f>) for f in staticFnames
printSpecs :: IO ()
printSpecs = do
  -- One dir f per post
  postDirs <- getPostDirs
  -- Still 1 dir per post but may be < f / body >
  sectionDirs <- mapM getSectionDir postDirs
  mapM_ displaySections sectionDirs

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
  mapM_ (printIndented . renderStatement) fnames

  -- <var f> <- makeFigure "80%" "Figure X:" <$> <renderAbs|renderPath> ("static" </> <f>)
  mapM_ (printIndented . renderStaticStatement) staticFnames

  -- (Just "<title f>", <var f>) for f in fnames
  -- (Nothing,        , <figure f>) for f in staticFnames
  let sections = (map sectionStatement fnames) ++ (map staticSectionStatement staticFnames)

  printIndented $ ("let body = " ++ listStr sections)
  case staticDir of
    Just _ -> do
      putStrLn "Don't forget to add:"
      printIndented "allStaticPaths <- listDirectoryRecursive $ bodyDir </> \"static\""
      printIndented "..."
      printIndented "staticPaths = allStaticPaths,"
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

-- Drop file number prefix and extension
dropFnameMeta :: String -> String
dropFnameMeta s =
  case break (== '-') s of
    (_, '-' : rest) -> dropExtension rest
    _ -> dropExtension s

-- String format conversions

-- "hello-world" -> "helloWorld"
-- Note kebabSplit always returns a populated list
--  (i.e., at least a singleton with the whole string)
kebabToCamel :: String -> String
kebabToCamel "" = ""
kebabToCamel s =
  case kebabSplit s of
    (x : xs) -> x ++ concatMap capitalise xs
    [] -> ""

-- "hello-world" -> "Hello World"
kebabToTitle :: String -> String
kebabToTitle s = unwords $
  case kebabSplit s of
    (x : xs) -> [capitalise x] ++ map titleCapitalise xs
    [] -> []

kebabSplit :: String -> [String]
kebabSplit xs =
  case break (== '-') xs of
    (before, '-' : rest) -> [before] ++ kebabSplit rest
    _ -> [xs]

capitalise :: String -> String
capitalise "" = ""
capitalise (x : xs) = toUpper x : xs

titleCapitalise :: String -> String
titleCapitalise w
  | w `elem` toKeepLowerCase = w
  | otherwise = capitalise w

toKeepLowerCase :: [String]
toKeepLowerCase =
  [ -- articles
    "a",
    "an",
    "the",
    -- conjunctions
    "and",
    "but",
    "or",
    "nor",
    "for",
    "yet",
    "so",
    -- prepositions
    "as",
    "at",
    "by",
    "in",
    "of",
    "on",
    "per",
    "to",
    "up",
    "via"
  ]

-- Pretty string representations of Haskell types

quote :: String -> String
quote s = "\"" ++ s ++ "\""

tupleStr :: [String] -> String
tupleStr lst = "(" ++ intercalate ", " lst ++ ")"

-- Make a list like
-- let lst =
--      [
--         el1,
--         el2
--      ]
listStr :: [String] -> String
listStr lst = left ++ elements ++ right
  where
    elements = intercalate ("," ++ space) lst
    left = space ++ "["
    right = space ++ "]"
    space = "\n" ++ tabs 4

printIndented :: String -> IO ()
printIndented s = putStrLn $ (tabs 1) ++ s

tabs :: Int -> String
tabs n = replicate (n * 2) ' '