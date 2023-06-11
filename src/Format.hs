{-# LANGUAGE ScopedTypeVariables #-}

module Format (run, format) where

import Control.Monad
import Data.Char (isSpace)
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
go insideInterpolate (line : rest)
  | isStarter line = line : go True rest
  | isEnd line = line : go False rest
  | insideInterpolate = ("  " <> line) : go True rest
  | otherwise = line : go insideInterpolate rest
go _ [] = []

isStarter :: String -> Bool
isStarter = (== "[i|") . trim

isEnd :: String -> Bool
isEnd = (== "|]") . trim

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
