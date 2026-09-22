module MakeMergedMds (generateMergedMds) where

import Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath (takeExtension, takeFileName, (</>))
import Utils (getFootnotesDir, getInnerFiles, getPostDirs, getSectionDir, safeCreateDir)

generateMergedMds :: IO ()
generateMergedMds = do
  postDirs <- getPostDirs

  for_ postDirs $ \x -> do
    dir <- getSectionDir x
    makeMerged (bodyMergedFile x) dir

  for_ postDirs $ \x -> do
    mdir <- getFootnotesDir x
    maybeDo (makeMerged (ftMergedFile x)) mdir
  where
    bodyMergedFile postDir = takeFileName postDir ++ ".md"
    ftMergedFile postDir = takeFileName postDir ++ "-ft.md"

    maybeDo = maybe doNothing
    doNothing = return ()

makeMerged :: String -> FilePath -> IO ()
makeMerged mergedFile dir = do
  paths <- getInnerFiles dir
  let mdFiles = filter ((== ".md") . takeExtension) paths
  contents <- mapM TIO.readFile mdFiles

  let merged = T.intercalate (T.pack "\n") contents
  let targetFolder = "misc" </> "merged"
  let targetPath = targetFolder </> mergedFile

  putStrLn targetPath

  safeCreateDir targetFolder
  TIO.writeFile targetPath merged