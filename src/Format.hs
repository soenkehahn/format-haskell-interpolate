{-# LANGUAGE ScopedTypeVariables #-}

module Format (run, format) where

import Control.Monad
import System.IO (readFile')
import WithCli

run :: IO ()
run = withCli $ \(files :: [FilePath]) -> do
  forM_ files $ \file -> do
    content <- readFile' file
    writeFile file $ format content

format :: String -> String
format = unlines . go False . lines

go :: Bool -> [String] -> [String]
go insideInterpolate (line : rest) = case line of
  "[i|" -> line : go True rest
  "|]" -> line : go False rest
  _ | insideInterpolate -> ("  " <> line) : go True rest
  _ -> line : go insideInterpolate rest
go _ [] = []
