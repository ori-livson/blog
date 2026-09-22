module Main where

import MakeMergedMds (generateMergedMds)
import PageSpecGenerator (printSpecs)

main :: IO ()
main = do
  putStrLn "Printing Specs"
  printSpecs
  putStrLn "Generating Merged MDs"
  generateMergedMds